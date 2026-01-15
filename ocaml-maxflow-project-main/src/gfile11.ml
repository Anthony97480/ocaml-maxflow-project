open Graph
open Printf
    
type path = string

(* Format of text files:
   % This is a comment

   % A node with its coordinates (which are not used), and its id.
   n 88.8 209.7 0
   n 408.9 183.0 1

   % Edges: e source dest label id  (the edge id is not used).
   e 3 1 11 0 
   e 0 2 8 1

*)

(* Compute arbitrary position for a node. Center is 300,300 *)
let iof = int_of_float
let foi = float_of_int

let index_i id = iof (sqrt (foi id *. 1.1))

let compute_x id = 20 + 180 * index_i id

let compute_y id =
  let i0 = index_i id in
  let delta = id - (i0 * i0 * 10 / 11) in
  let sgn = if delta mod 2 = 0 then -1 else 1 in

  300 + sgn * (delta / 2) * 100
  

let write_file path graph =

  (* Open a write-file. *)
  let ff = open_out path in

  (* Write in this file. *)
  fprintf ff "%% This is a graph.\n\n" ;

  (* Write all nodes (with fake coordinates) *)
  n_iter_sorted graph (fun id -> fprintf ff "n %d %d %d\n" (compute_x id) (compute_y id) id) ;
  fprintf ff "\n" ;

  (* Write all arcs *)
  let _ = e_fold graph (fun count arc -> fprintf ff "e %d %d %d %s\n" arc.src arc.tgt count arc.lbl ; count + 1) 0 in
  
  fprintf ff "\n%% End of graph\n" ;
  
  close_out ff ;
  ()

(* Reads a line with a node. *)
let read_node graph line =
  try Scanf.sscanf line "n %f %f %d" (fun _ _ id -> new_node graph id)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* Ensure that the given node exists in the graph. If not, create it. 
 * (Necessary because the website we use to create online graphs does not generate correct files when some nodes have been deleted.) *)
let ensure graph id = if node_exists graph id then graph else new_node graph id

(* Reads a line with an arc. *)
let read_arc graph line =
  try Scanf.sscanf line "e %d %d %_d %s@%%"
        (fun src tgt lbl -> let lbl = String.trim lbl in new_arc (ensure (ensure graph src) tgt) { src ; tgt ; lbl } )
  with e ->
    Printf.printf "Cannot read arc in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* Reads a comment or fail. *)
let read_comment graph line =
  try Scanf.sscanf line " %%" graph
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line ;
    failwith "from_file"

let from_file path =

  let infile = open_in path in

  (* Read all lines until end of file. *)
  let rec loop graph =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let graph2 =
        (* Ignore empty lines *)
        if line = "" then graph

        (* The first character of a line determines its content : n or e. *)
        else match line.[0] with
          | 'n' -> read_node graph line
          | 'e' -> read_arc graph line

          (* It should be a comment, otherwise we complain. *)
          | _ -> read_comment graph line
      in      
      loop graph2

    with End_of_file -> graph (* Done *)
  in

  let final_graph = loop empty_graph in
  
  close_in infile ;
  final_graph


let f_add_list l element =
  element::l

let rec get_str_arc (l: 'a arc list) =
  match l with
  | [] -> ""
  | x::rest -> (string_of_int x.src) ^ " -> " ^ (string_of_int x.tgt) ^ "[label = \"" ^ (string_of_int x.lbl) ^ "\" ];\n" ^ get_str_arc rest

let export_id_graph (filename: string) (graph: 'a graph) =
  let file = open_out filename in
  let arc_list = e_fold graph f_add_list [] in
  fprintf file "%s" ("digraph mongraph {\nnode [shape = circle];\n" ^
    (get_str_arc arc_list) ^
  "}");
  close_out file

let export (path: path) (graph: path graph) =
  let file = open_out path in
  fprintf file "digraph mongraph {\nnode [shape = circle];\n";
  let _ = e_fold graph (fun count arc -> fprintf file "%d -> %d [label = \"%s\"];\n" arc.src arc.tgt arc.lbl ; count + 1) 0 in
  fprintf file "}";
  close_out file;
  ()

let rec concat_id_list (id_list: id list) (acu: string) =
  match id_list with
  | [] -> acu
  | x::rest -> concat_id_list rest (acu^(string_of_int x)^"/")

let export_file_list (path: path) (id_list: id list) =
  let file = open_out path in
  let string_list = concat_id_list id_list "" in
  fprintf file "test find_path: %s" (string_list)

let export_file (path: path) (id: id) =
  let file = open_out path in
  fprintf file "test max flow: %s" (string_of_int id)