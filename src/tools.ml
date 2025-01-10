(* Yes, we have to repeat open Graph. *)
open Graph

(*val n_fold: 'a graph -> ('b -> id -> 'b) -> 'b -> 'b*)
(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes (gr:'a graph) = n_fold gr (fun acu id -> new_node acu id) empty_graph

let gmap (gr: 'a graph) f = e_fold gr (fun g arc -> (new_arc g {arc with lbl = f arc.lbl})) (clone_nodes gr)

let add_arc g id1 id2 n = 
  let exists = find_arc g id1 id2 in
  match exists with
  | None -> new_arc g {src = id1; tgt = id2; lbl = n}
  | Some arc -> new_arc g {src = arc.src; tgt = arc.tgt; lbl = arc.lbl + n}
