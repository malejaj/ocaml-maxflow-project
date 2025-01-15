open Graph

let init_graph_with_arcs nodes =
  let rec init_nodes graph = function
    | [] -> graph
    | node :: rest -> init_nodes (new_node graph node) rest
  in
  let init_graph = init_nodes empty_graph nodes in

  let rec init_arc current_graph current_node nodes =
    match nodes with
    | [] -> current_graph
    | tgt :: rest ->
        let forward_arc = new_arc current_graph {src = current_node; tgt; lbl = 100000} in
        let both_arcs = new_arc forward_arc {src = tgt; tgt = current_node; lbl = 100000} in
        init_arc both_arcs current_node rest
  in
  let rec init_all_arcs graph = function
    | [] -> graph
    | node :: rest -> let updated_graph = init_arc graph node rest in
      init_all_arcs updated_graph rest
    in init_all_arcs init_graph nodes


let final_graph g l = 
  let new_graph = new_node (new_node g (-1)) (-2) in
  let rec loop graph l =  
    match l with 
    | []->graph
    | (id, cost) :: rest -> if cost <= 0 then loop (new_arc graph {src=(-2); tgt = id; lbl = (-cost)}) rest
    else loop (new_arc graph {src= id; tgt = (-1); lbl = cost}) rest in
    loop new_graph l
;;
