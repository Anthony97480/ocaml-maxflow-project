open Graph
open Tools

(* Initialisation du graphe avec des étiquettes (flux, capacité) *)
let initialize graph = gmap graph (fun capacity -> (0, capacity))

let rec find_path graph visited noeud_source noeud_target =
  if noeud_source = noeud_target then Some [noeud_target]
  else
    match out_arcs graph noeud_source with
    | [] -> None
    | arcs ->
      List.fold_left
        (fun acc arc ->
          let residual_capacity = snd arc.lbl - fst arc.lbl in
          if List.mem arc.tgt visited || residual_capacity <= 0 then acc
          else
            match find_path graph (arc.tgt :: visited) arc.tgt noeud_target with
            | None -> acc
            | Some path -> Some (noeud_source :: path))
        None arcs

(* Trouve la capacité minimale sur un chemin donné *)
let rec min_capacity graph path =
  match path with
  | [] | [_] -> max_int
  | src :: (dst :: _ as rest) ->
    let arc_capacity =
      match find_arc graph src dst with
      | Some { lbl = (lbl_flow, lbl_capacity); _ } -> lbl_capacity - lbl_flow
      | None -> 0
    in
    min arc_capacity (min_capacity graph rest)


let rec update_flow graph path flow =
  match path with
  | [] | [_] -> graph
  | src :: (dst :: _ as rest) ->
    (* Mettre à jour l'arc direct *)
    let graph =
      add_arc graph src dst (flow, 0)
    in
    (* Mettre à jour l'arc inverse *)
    let graph =
      add_arc graph dst src (-flow, 0)
    in
    update_flow graph rest flow    


(* Algorithme de Ford-Fulkerson *)
let rec ford_fulkerson graph source sink =
  match find_path graph [] source sink with
  | None -> (0, graph)
  | Some path ->
    let flow = min_capacity graph path in
    let graph = update_flow graph path flow in
    let (max_flow, final_graph) = ford_fulkerson graph source sink in
    (flow + max_flow, final_graph)

