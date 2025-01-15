open Graph
open Tools

let rec find_full_path g visited src tgt =
  if src = tgt then [src]
  else
    let rec arcs_route = function
      | [] -> []
      | {src = _; tgt = dst; lbl = curr} :: rest ->
          if not (List.mem dst visited) && curr > 0 then dst :: arcs_route rest
          else arcs_route rest
    in
    let neighbors = arcs_route (out_arcs g src) in
    let rec explore = function
      | [] -> []
      | node :: rest -> let path = find_full_path g (node :: visited) node tgt in
          if path = [] then explore rest
          else src :: path
    in
    explore neighbors
    

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
      (fun graph {src = source; tgt = target; lbl = label} -> 
        match find_arc existing_residual_graph source target with
        | None -> raise (Failure "There must be an arc between them")
        | Some found_arc -> 
            new_arc graph { 
              src = source; 
              tgt = target; 
              lbl = (string_of_int (label - found_arc.lbl) ^ "/" ^ string_of_int label) 
            }
      ) build_new  


let ford_fulkerson g src tgt = 
  let rec loop existing_residual_graph = 
    let max_flow = 500 in
    let full_path = find_full_path existing_residual_graph [] src tgt in
    Printf.printf "\n";
    match full_path with
    | [] -> create_residual_graph g existing_residual_graph
    | l ->
        let flow = max_flow_path existing_residual_graph max_flow l in
        let updated_residual_graph = update_node_flow existing_residual_graph flow l in
        loop updated_residual_graph
  in
  loop g
;;
