open Graph

(*&& (int_lbl<>0)*)

(*Même logique qu'en prologue:
has_path(X, Y):-
  connected(X, Z), //act connecter
  has_path(Z, Y).
*)
let rec find_path (graph: 'a graph) (s: id) (e: id) (arc_list: 'a arc list) (visited: id list) =
  match arc_list with
  | [] -> None
  | x :: rest when x.lbl = "0" ->
      find_path graph s e rest visited
  | x :: rest when List.mem x.tgt visited ->
      find_path graph s e rest visited
  | x :: _ when x.tgt = e ->
      Some (s::x.tgt::[])
  | x :: rest ->
      match find_path graph x.tgt e (out_arcs graph x.tgt) (x.tgt :: visited) with
      | None ->
          find_path graph s e rest visited
      | Some paths ->
          Some (s :: paths)

(*
  pour tous: les sommet du graphique
  mettre le flow à 0
  tant qu'il: peut exister des chemin différent
  alors: trouver un chemin de first a last
  envoyer une quantité d'info correspondant au minimul disponible sur le chemin
  augmenter le flow de la valeur envoyer sur ce chemin pour ce sommet

*)
let rec max_sending_flow (gr: 'a graph) (id_list: id list) (acu: id) =
  match id_list with
    | [] | [_] -> acu
    | x::y::rest -> let arc_xy = find_arc gr x y in (
        match arc_xy with
          | None -> acu
          | Some arc -> let capacity = int_of_string arc.lbl in
            let new_acu = if acu=0 then capacity else min acu capacity in max_sending_flow gr (y::rest) new_acu
    )
    

let rec send_info (gr: string graph) (id_list: id list) (msg_size: int) =
  match id_list with
    | [] | [_] -> gr
    | x::y::rest -> let arc_xy = find_arc gr x y in (
      match arc_xy with
        | None -> gr
        | Some arc -> let new_lbl = ( (int_of_string arc.lbl) - msg_size) in 
          let update_arc = {src=arc.src; tgt=arc.tgt; lbl=(string_of_int new_lbl)} in 
          send_info (new_arc gr update_arc) rest msg_size)


  let rec ffalgo (gr: string graph) (s: id) (e: id) =
  let arc_list = out_arcs gr s in
  match find_path gr s e arc_list [s] with
  | None -> gr
  | Some id_list -> let flow = max_sending_flow gr id_list 0 in
    if flow = 0 then gr else let new_graph = send_info gr id_list flow in
      ffalgo new_graph s e