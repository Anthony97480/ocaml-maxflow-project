open Graph
open Gfile
open Tools
open Algo


(* Convertit un graphe avec des étiquettes de type string en un graphe avec des étiquettes de type int *)
let g_to_int (gr: string graph) : int graph =
  gmap gr int_of_string

let g_to_string (gr: (int * int) graph) : string graph =
  gmap gr (fun (flow, capacity) -> "(" ^ string_of_int flow ^ ", " ^ string_of_int capacity ^ ")")
 


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

  (* Lit le graphe depuis le fichier d'entrée *)
  let graph = from_file infile in
  
  (* Convertit le graphe en un graphe avec des étiquettes entières *)
  let initGraph = g_to_int graph in

  (* Exécute l'algorithme de Ford-Fulkerson *)
  let (flow, finalGraph) = ford_fulkerson (initialize initGraph ) _source _sink in
  
  (* Affiche le flux maximum calculé *)
  Printf.printf "Maximum flow: %d\n" flow;

  (* Convertit le graphe final en string graph avant exportation *)
  let finalGraphString = g_to_string finalGraph in

  (* Exporte le graphe final au format DOT *)
  export outfile finalGraphString;

  Printf.printf "Graph exported to %s\n" outfile;




