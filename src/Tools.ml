open Graph

(*assert false is of type Va.a, so the type checker is happy*)

let f_add_list l element =
  element::l

let close_nodes (gr: 'a graph) =
  n_fold gr new_node empty_graph

let gmap (gr: 'a graph) f = 
  let arc_list = e_fold gr f_add_list [] in
  let map_arc = List.map f arc_list in
  List.fold_left new_arc empty_graph map_arc

let add_arc gr id1 id2 n =
  let arc_find = find_arc gr id1 id2 in
  match arc_find with
  | None -> 
  | Some arc_opt -> let arc = {src=arc_opt.src; tgt=arc_opt.tgt; lbl=arc_opt.lbl + n} in new_arc gr arc

(*Replace _gr and _f by gr and f when you start writing the real function*)

