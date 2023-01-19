open Lib.Static


(* open Pycaml.Ast *)


let filename = Stdlib.Sys.argv.(1) 
let json = Yojson.Basic.from_file filename

let static = anal json 

let () = print_map static















