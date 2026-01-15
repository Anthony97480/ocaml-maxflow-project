open Gfile
open Graph
open FordF
open Tools
    
let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in
 
  let initial_graph = from_file infile in

  (* Convertit les labels du graphe initial en int pour l'algorithme (si nécessaire, mais FordF utilise déjà des strings) *)
  (* Pour FordF, les labels sont déjà des strings de capacités. *)
  (* Gardons les labels en string, et convertissons les int_of_string au besoin *)

  let result_graph = ffalgo initial_graph _source _sink in

  (* Fonction pour déduire le flux et créer un graphe lisible *)
  let deduce_flow_graph (original_gr: string graph) (final_res_gr: string graph) : string graph =
    let flow_gr = clone_nodes original_gr in
    let add_flow_arc_to_deduced_graph g original_arc =
      let original_capacity = int_of_string original_arc.lbl in
      match find_arc final_res_gr original_arc.src original_arc.tgt with
      | Some final_res_arc ->
          let final_residual_capacity = int_of_string final_res_arc.lbl in
          let flow_on_arc = original_capacity - final_residual_capacity in
          if flow_on_arc > 0 then
            new_arc g { src = original_arc.src; tgt = original_arc.tgt; lbl = string_of_int flow_on_arc }
          else g
      | None -> (* If arc doesn't exist in residual graph, it means flow saturated it completely and was then 'removed' for some reason, or it's a backward edge. *)
                (* Or it's a fully saturated edge that might have been removed or whose residual capacity is 0 *)
                if original_capacity > 0 then (* if it was an original arc with capacity *)
                    (* if it's not in the residual graph or its residual capacity is 0, then flow is original capacity *)
                    new_arc g {src = original_arc.src; tgt = original_arc.tgt; lbl = string_of_int original_capacity}
                else g
    in
    e_fold original_gr add_flow_arc_to_deduced_graph flow_gr
  in

  let flow_graph_to_export = deduce_flow_graph initial_graph result_graph in
  
  (* Exporte le graphe de flux final au format DOT *)
  export outfile flow_graph_to_export;
  export "./graph2.dot" flow_graph_to_export;