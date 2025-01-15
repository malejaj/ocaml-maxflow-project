open Gfile
open Fordfulkerson
open Moneysharing
    
let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in
  (* let runff = ford_fulkerson (gmap graph  (fun x -> int_of_string x)) _source _sink  in *)

  let people = [0; 1; 2] in
  let fully_connected_graph = init_graph_with_arcs people in
  let fg = final_graph fully_connected_graph [(0, 20); (1, -10); (2, -10)] in 
  let runff = ford_fulkerson fg (-2) (-1) in 

  (* Rewrite the graph that has been read. *)
  let () = write_file outfile runff in
    export "graph_infile" graph;
    export "answer" runff;
  ()