open Graph
open Tools


type paths = id list

let rec find_full_path_helper g visited src tgt = 
  if src = tgt
    then Some []
else 
  let out = out_arcs g src in
  let is_visited node visited = List.mem node visited in
  let rec arcs_route = function
  | [] -> []
  | {src = _; tgt = dst; lbl = curr} :: rest -> if is_visited dst visited || curr <= 0
    then arcs_route rest
    else dst :: arcs_route rest
  in
  match arcs_route out with
  | [] -> None
  | node :: _ -> match find_full_path_helper g (node :: visited) node tgt with
    | None -> find_full_path_helper g (node :: visited) src tgt
    | Some l -> Some(node :: l)

let find_full_path g visited src tgt = 
  match (find_full_path_helper g visited src tgt) with
    | None -> None
    | Some l -> Some (src :: l)
  
let rec max_flow_path g curr_max = function
  | [] -> curr_max
  | _ :: [] -> curr_max
  | n1 :: n2 :: rest ->
    match find_arc g n1 n2 with
    | Some arc -> max_flow_path g (min curr_max arc.lbl) (n2 :: rest)
    | None -> 0

let rec update_node_flow g value = function
  | [] -> g
  | _ :: [] -> g
  | n1 :: n2 :: rest -> update_node_flow (add_arc g n1 n2 (-value)) value (n2 :: rest)

let create_residual_graph g existing_residual_graph = 
  let build_new = clone_nodes g in
  e_fold g 
  (
    fun graph {src = source; tgt= target;  lbl = label} -> 
      let v = match find_arc existing_residual_graph source target with
      | None -> raise (Failure "Error")
      | Some z -> z
    in
    new_arc graph { src = source ; tgt = target ; lbl = (string_of_int (label - v.lbl) ^ "/" ^ string_of_int label)}
  ) build_new


let ford_fulkerson g src tgt = 
  let rec loop existing_residual_graph src tgt = 
    let max_flow = 500 in
    let visited = [] in 
    let full_path = find_full_path existing_residual_graph visited src tgt in
    Printf.printf "\n";
    match full_path with
      | None -> create_residual_graph g existing_residual_graph
      | Some l ->
        let flow = max_flow_path existing_residual_graph max_flow l in
        let updated_residual_graph = update_node_flow existing_residual_graph flow l in
        loop updated_residual_graph src tgt
      in
      loop g src tgt
;;