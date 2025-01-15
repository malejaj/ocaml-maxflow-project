open Graph

val find_full_path : int graph -> int list -> id -> id -> id list

val max_flow_path : int graph -> int -> id list -> int

val update_node_flow : int graph -> int -> id list -> int graph

val create_residual_graph : int graph -> int graph -> string graph

val ford_fulkerson : int graph -> id -> id -> string graph
