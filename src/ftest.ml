open Gfile
open Graph
open FordF
    
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
 
  (* Open file *)
  let graph = from_file infile in
  (* Rewrite the graph that has been read. *)
  let () = write_file outfile graph in
  let new_graph = ffalgo graph 0 7 in
  let id_list = find_path graph 0 7 (out_arcs graph 0) [0] in
  match id_list with
  | None -> Printf.printf "raté";
  | Some id -> let () = export_file_list "./id list.txt" id in
  let test_maxflow = max_sending_flow graph id max_int in
  match test_maxflow with
  | 0 -> Printf.printf "raté";
  | x -> let () = export_file "./flow.txt" x in
  let () = export "./new_graph10.dot" new_graph in
  let () = export "./graph10.dot" graph in ()
