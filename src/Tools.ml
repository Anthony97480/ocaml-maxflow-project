open Graph

(*assert false is of type Va.a, so the type checker is happy*)

let f_add_list l element =
  element::l

let get_arc_list gr =
  e_fold gr f_add_list []

let close_nodes (gr: 'a graph) =
  n_fold gr new_node empty_graph

let gmap (gr: 'a graph) f = 
  let arc_list = get_arc_list gr in
  let map_arc = List.map f arc_list in
  List.fold_left new_arc empty_graph map_arc

let add_arc gr id1 id2 n =
  let arc_find = find_arc gr id1 id2 in
  match arc_find with
  | None -> (let arc = {src=id1; tgt=id2; lbl=n} in new_arc gr arc)
  | Some arc_opt -> (let arc = {src=arc_opt.src; tgt=arc_opt.tgt; lbl=arc_opt.lbl + n} in new_arc gr arc)


(*Replace _gr and _f by gr and f when you start writing the real function*)

let rec find_start_list (arc_list: 'a arc list) (s: id) =
  match arc_list with
  | [] -> None
  | x::rest -> if (x.src = s) then Some x else find_start_list rest s

let rec find_return_arc arc_list arc =
  match (arc_list, arc) with
  | ([], _) -> None
  | (_, None) -> None
  | (x::rest, Some arc_val) -> if (x.src = arc_val.tgt) && (x.tgt = arc_val.src) then Some x else find_return_arc rest arc


let rec return_arc_graph (gr: 'a graph) (arc_list: 'a arc list) =
  match arc_list with
  | [] -> gr
  | x::rest -> let arc = find_arc gr x.tgt x.src in (
    match arc with
      | None -> let new_graph = add_arc gr x.tgt x.src 0 in (return_arc_graph new_graph rest)
      | Some _ -> return_arc_graph gr rest
    )

let find_path (graph: 'a graph) (s: id) (e: id) =
  let arc_list = get_arc_list graph in
  let start_arc = find_start_list arc_list s in
  let new_graph = return_arc_graph graph arc_list in 
  let return_arc_list = get_arc_list new_graph in
  let return_start_arc = find_return_arc return_arc_list start_arc in

  let exit = 0 in
  while (exit=0) do
    match (start_arc, return_start_arc) with
    | (None, _) -> ()
    | (_, None) -> ()
    | (Some s_arc_opt, Some r_arc_opt) -> if (s_arc_opt.tgt = e) && (r_arc_opt.lbl = 0) then (let path_list = f_add_list [] start_arc in
    ()) else ()
  done
  

(*
  pour tous: les sommet du graphique
  mettre le flow à 0
  tant qu'il: peut exister des chemin différent
  alors: trouver un chemin de first a last
  envoyer une quantité d'info correspondant au minimul disponible sur le chemin
  augmenter le flow de la valeur envoyer sur ce chemin pour ce sommet

*)
let ffalgo (gr: 'a graph) (s: id) (e: id) =
  let arc_list = e_fold gr f_add_list [] in 
  let s_to_e_path = find_path gr s e in ()
