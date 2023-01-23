open Lib.Static


(* open Pycaml.Ast *)


let filename = Stdlib.Sys.argv.(1) 
let json = Yojson.Basic.from_file filename

let static = anal json 

let () = Stdio.print_endline "*****static*****" ; TEnv.print_map static(* Print for test *)















