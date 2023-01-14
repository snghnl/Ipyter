open Pycaml
open Static





let filename = Stdlib.Sys.argv.(1) 
let json = Yojson.Basic.from_file filename;;

Dynamic.json2tracebacks json |> Print.print_tracebacks 





