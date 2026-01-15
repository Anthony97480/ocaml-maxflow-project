open Graph

(* Trouve un chemin augmentant dans le graphe résiduel *)
val find_path : (int * int) Graph.graph -> int list -> int -> int -> int list option

(* Trouve la capacité minimale sur un chemin donné *)
val min_capacity : (int * int )graph -> id list -> int

(* Met à jour les capacités des arcs le long du chemin *)
val update_flow : (int * int) Graph.graph -> int list -> int -> (int * int) Graph.graph

(* Algorithme de Ford-Fulkerson *)
val ford_fulkerson : (int * int) Graph.graph -> int -> int -> int * (int * int) Graph.graph


val initialize: int graph -> (int*int) graph
