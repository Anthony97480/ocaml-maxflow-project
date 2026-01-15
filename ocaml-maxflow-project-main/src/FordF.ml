open Graph

(*Même logique qu'en prologue:
has_path(X, Y):-
  connected(X, Z), //act connecter
  has_path(Z, Y).
*)
let rec find_path_aux graph visited current_node sink =
  if current_node = sink then
    Some [sink]
  else
    let new_visited = current_node :: visited in
    let traversable_arcs =
      try
        List.filter
          (fun arc -> arc.lbl <> "0" && not (List.mem arc.tgt new_visited))
          (out_arcs graph current_node)
      with Graph_error _ -> []
    in
    let rec find_from_arcs arcs =
      match arcs with
      | [] -> None
      | arc :: rest ->
          match find_path_aux graph new_visited arc.tgt sink with
          | Some path -> Some (current_node :: path)
          | None -> find_from_arcs rest
    in
    find_from_arcs traversable_arcs

let find_path graph source sink =
  find_path_aux graph [] source sink


(*
  pour tous: les sommet du graphique
  mettre le flow à 0
  tant qu'il: peut exister des chemin différent
  alors: trouver un chemin de first a last
  envoyer une quantité d'info correspondant au minimul disponible sur le chemin
  augmenter le flow de la valeur envoyer sur ce chemin pour ce sommet

*)
let rec max_sending_flow (gr: string graph) (id_list: id list) (acu: id) =
  match id_list with
    | [] | [_] -> acu
    | x::y::rest -> let arc_xy = find_arc gr x y in (
        match arc_xy with
          | None -> acu
          | Some arc -> let capacity = (int_of_string arc.lbl) in
            let new_acu = if acu=0 then capacity else min acu capacity in max_sending_flow gr (y::rest) new_acu
    )
    

let rec send_info (gr: string graph) (id_list: id list) (msg_size: int) =
  match id_list with
  | [] | [_] -> gr
  | x :: y :: rest -> let gr_flow = (
    match find_arc gr x y with
      | None -> gr
      | Some arc -> let new_lbl = ((int_of_string arc.lbl) - msg_size) in
        let updated_arc = { src = x; tgt = y; lbl = (string_of_int new_lbl) } in new_arc gr updated_arc ) in
    let gr_back = (
      match find_arc gr_flow y x with
        | Some arc_back -> let new_flow = ((int_of_string arc_back.lbl) + msg_size ) in
          let updated_back = { src = y; tgt = x; lbl = string_of_int new_flow } in new_arc gr_flow updated_back
        | None -> let back_arc = { src = y; tgt = x; lbl = string_of_int msg_size } in new_arc gr_flow back_arc) in
      send_info gr_back (y :: rest) msg_size


  let rec ffalgo (gr: string graph) (s: id) (e: id) =
  match find_path gr s e with
  | None -> gr
  | Some id_list -> let flow = max_sending_flow gr id_list 0 in
    if flow = 0 then gr else let new_graph = send_info gr id_list flow in
      ffalgo new_graph s e