open Graph

type paths = id list
val find_full_path_helper: int graph -> int list -> id -> id -> paths option

val find_full_path: int graph -> int  list -> id -> id -> paths option

val max_flow_path: int graph -> int -> paths -> int

val update_node_flow: int graph -> int -> paths -> int graph

val create_residual_graph  : int graph -> int graph -> string graph

val ford_fulkerson : int graph -> id -> id -> string graph