open Printf 
open Graph 
open Algo 
open Str
open Tools
open Gfile

type team_stats = { id: int; wins: int; losses: int; som_matchs: int; against_matchs: int list; }
type game = { t1: int; t2: int; n: int; } 

let g_to_string_int (gr: int graph) : string graph =
  gmap gr string_of_int


let rec map_str_to_int_list l = match l with 
  | [] -> [] 
  | h :: t -> (int_of_string h) :: (map_str_to_int_list t)

let rec reverse_list_acc acc l = match l with 
  | [] -> acc 
  | h :: t -> reverse_list_acc (h :: acc) t 

let reverse_list l = reverse_list_acc [] l 

let rec find_team_out max_wins teams = match teams with 
  | [] -> None 
  | team :: rest -> if team.wins > max_wins then Some team else find_team_out max_wins rest 

let rec game_list_team1 t1_id index matchs = match matchs with 
  | [] -> [] 
  | num_games :: rest -> { t1 = t1_id; t2 = index; n = num_games } :: game_list_team1 t1_id (index + 1) rest 

let rec filter_game_team games_list other_teams = match games_list with 
  | [] -> [] 
  | game :: rest -> let is_ok = game.t2 > game.t1 && List.exists (fun t -> t.id = game.t2) other_teams in 
    let rest = filter_game_team rest other_teams in 
    if is_ok then game :: rest else rest 

let rec find_team teams id_to_find = match teams with 
  | [] -> raise Not_found 
  | team :: rest -> if team.id = id_to_find then team else find_team rest id_to_find 

let rec team_out teams id_to_remove acc = match teams with 
  | [] -> reverse_list acc 
  | team :: rest -> if team.id <> id_to_remove then team_out rest id_to_remove (team :: acc) else team_out rest id_to_remove acc 

let filter_out_team_by_id teams id = team_out teams id [] 

let parse_line line = 
  let parts = split (Str.regexp "[ ]+") line in 
  match parts with 
  | "t" :: id_s :: wins_s :: losses_s :: rem_s :: against_s -> { id = int_of_string id_s; wins = int_of_string wins_s; losses = int_of_string losses_s; som_matchs = int_of_string rem_s; against_matchs = map_str_to_int_list against_s; } 
  | _ -> failwith ("Malformed line (expected 't ...'): " ^ line) 

let from_file path = 
  let infile = open_in path in 
  let rec loop acc = try 
      let line = input_line infile in 
      let stats = parse_line (String.trim line) in 
      loop (stats :: acc) with End_of_file -> close_in infile; reverse_list acc in 
  loop [] 

let facile_elim team_x other_teams = 
  let max_wins_x = team_x.wins + team_x.som_matchs in 
  match find_team_out max_wins_x other_teams with 
  | Some other_team -> printf "Team %d is trivially eliminated. Team %d already has %d wins, while team %d can at best reach %d.\n" team_x.id other_team.id other_team.wins team_x.id max_wins_x; true 
  | None -> false 

let games_t1_t2 other_teams = 
  let rec collect_from_teams acc teams_to_process = match teams_to_process with 
    | [] -> acc 
    | t1 :: rest -> let all_potential_games = game_list_team1 t1.id 0 t1.against_matchs in 
      let valid_games = filter_game_team all_potential_games other_teams in 
      collect_from_teams (valid_games @ acc) rest in 
  collect_from_teams [] other_teams 

let create_base_graph source_id sink_id other_teams = 
  let rec aux graph_acc teams_list = match teams_list with 
    | [] -> graph_acc 
    | team :: rest -> let new_graph = new_node graph_acc team.id in 
      aux new_graph rest in 
  let initial_graph = new_node (new_node empty_graph source_id) sink_id in 
  aux initial_graph other_teams 

let add_game_arcs graph games_to_play source_id game_node_id_offset = 
  let rec aux (graph_acc, flow_acc) games_list = match games_list with 
    | [] -> (graph_acc, flow_acc) 
    | game :: rest -> let game_id = game_node_id_offset + (game.t1 * 10) + game.t2 in 
      let new_flow = flow_acc + game.n in 
      let gr' = new_node graph_acc game_id in 
      let gr'' = new_arc gr' { src = source_id; tgt = game_id; lbl = game.n } in 
      let gr''' = new_arc gr'' { src = game_id; tgt = game.t1; lbl = 9999 } in 
      let new_graph = new_arc gr''' { src = game_id; tgt = game.t2; lbl = 9999 } in 
      aux (new_graph, new_flow) rest in 
  aux (graph, 0) games_to_play 

let add_team_arcs graph other_teams sink_id max_wins_x = 
  let rec aux graph_acc teams_list = match teams_list with 
    | [] -> graph_acc 
    | team_i :: rest -> let capacity = max_wins_x - team_i.wins in 
      let new_graph = new_arc graph_acc { src = team_i.id; tgt = sink_id; lbl = capacity } in 
      aux new_graph rest in 
  aux graph other_teams 

let build_flow_network team_x other_teams = 
  let max_wins_x = team_x.wins + team_x.som_matchs in 
  printf "Constructing flow network to check elimination of team %d (max wins: %d)\n" team_x.id max_wins_x; 
  let source_id = 1000 in 
  let sink_id = 1001 in 
  let game_node_id_offset = 2000 in 
  let base_graph = create_base_graph source_id sink_id other_teams in 
  let games_to_play = games_t1_t2 other_teams in 
  let (graph_with_game_arcs, total_games_flow) = add_game_arcs base_graph games_to_play source_id game_node_id_offset in 
  let final_graph = add_team_arcs graph_with_game_arcs other_teams sink_id max_wins_x in 
  (final_graph, source_id, sink_id, total_games_flow) 

let interpret_max_flow max_flow total_games_flow team_id = 
  printf "Total som_matchs games between other teams: %d\n" total_games_flow; 
  printf "Max flow computed: %d\n" max_flow; 
  if max_flow < total_games_flow then ( 
    printf "Team %d is eliminated.\n" team_id; true ) 
  else ( 
    printf "Team %d is not eliminated.\n" team_id; false ) 

let check_elimination teams team_to_check_id outfile_opt = 
  let team_x = find_team teams team_to_check_id in 
  let other_teams = filter_out_team_by_id teams team_to_check_id in 
  if facile_elim team_x other_teams then true 
  else 
    let (final_graph, source_id, sink_id, total_games_flow) = build_flow_network team_x other_teams in
    (match outfile_opt with
     | Some outfile ->
       let string_graph = g_to_string_int final_graph in
       export outfile string_graph
     | None -> ());
    let (max_flow, _) = ford_fulkerson (initialize final_graph) source_id sink_id in 
    interpret_max_flow max_flow total_games_flow team_to_check_id 

let () = 
  if Array.length Sys.argv < 3 || Array.length Sys.argv > 4 then begin 
    printf "\nUsage: %s infile team_id_to_check [outfile]\n\n" Sys.argv.(0); 
    printf " infile: file describing the cricket tournament\n"; 
    printf " team_id_to_check: the ID of the team to check for elimination\n";
    printf " outfile: optional DOT file to export the graph\n\n";
    exit 0 
  end; 

  let infile = Sys.argv.(1) in 
  let team_to_check_id = int_of_string Sys.argv.(2) in
  let outfile_opt = if Array.length Sys.argv = 4 then Some Sys.argv.(3) else None in
  printf "Reading tournament data from: %s\n" infile; 
  let teams = from_file infile in 
  if not (List.exists (fun t -> t.id = team_to_check_id) teams) then (printf "Error: Team with ID %d not found in file.\n" team_to_check_id; exit 1); 
  let _ = check_elimination teams team_to_check_id outfile_opt in ()