open Pycaml.Ast
open Base
open Yojson.Basic.Util 
open Pycaml
open Core



type pgm = Program of modul | None  

(* member "alias" for differentiating same-named variables *)
type variable = Var of { name : identifier ; alias: identifier ; meta: meta_data }

and meta_data = Meta of {filename: identifier ; classname: identifier ; funcname: identifier }

and var_type = 
| VInt
| VFloat
| VString 
| VBool
| VDict
| VList
| VClass of string
| VMethod
| VNone
| VEllipsis 



(* const2type: function that constant type to var_type *)
let const2type: constant -> var_type
= fun const ->
  match const with 
  | CInt _ -> VInt 
  | CFloat _ -> VFloat
  | CString _ -> VString
  | CBool _ -> VBool
  | CNone -> VNone
  | CEllipsis -> VEllipsis 

(* string2type: function that convert string to var_type *)
and string2type : string -> var_type
= fun types -> 
  match types with 
  | "int" -> VInt
  | "str" -> VString
  | "float" -> VFloat
  | "bool" -> VBool
  | "Dict" -> VDict
  | "List" -> VList
  | "method" -> VMethod
  | x -> if Stdlib.String.starts_with ~prefix:"Dict" x then VDict else VClass x

and type2string : var_type -> string
    = function 
      | VInt -> "int"
      | VFloat -> "float"
      | VString -> "str"
      | VBool -> "bool"
      | VDict -> "Dict"
      | VList -> "List"
      | VClass x -> x
      | VMethod -> "method"
      | VNone -> "None"
      | VEllipsis -> "..."


let generate_varName : unit -> identifier
= let varName_num = ref 0 in 
   fun () -> (varName_num := !varName_num + 1 ; ("t" ^ Int.to_string !varName_num))


let filename2pgm : identifier -> stmt list
= fun filename -> 
(* link to file *)
  let _ = Stdlib.Sys.command ("python3.10 pycaml/ast2json.py " ^ filename ^ " > /tmp/pgm.json") in 
    let json = Yojson.Basic.from_file "/tmp/pgm.json" in 
      let modul = Json2ast.to_module json in 
        match modul with 
        | Module x -> x.body
        | _ -> raise (Failure "Undefined Program")

(* **************************** *)
(* Module: Type Environment Map *)
(* **************************** *)

module Var2valMap = struct 
  module T = struct
    type t = variable
    let compare t1 t2 = 
      match t1, t2 with 
      | Var x, Var y -> String.compare x.alias y.alias
    let sexp_of_t t : Sexp.t = 
      match t with 
      | Var x -> Atom x.alias
  end

  include T
  include Comparator.Make(T)

end 




(* *************** *)
(* Module: NegType *)
(* *************** *)

module NegType = struct
  (* type var = identifier *)
  (* type candVars = var list *)
  type value = var_type list
  type negType = value Map.M (Var2valMap).t
  type traceback = Traceback of {filename: string ; classname: identifier ; funcname: identifier ; lineno : int ; args: negType} 
  type tracebacks = traceback list
 
  

  let to_args : Yojson.Basic.t -> meta_data -> negType 
  = fun json meta -> 
    to_assoc json 
    |> List.map ~f: (fun (a, b) -> (Var{ name = a ; alias = generate_varName() ; meta = meta}, 
      to_list b 
      |> List.map ~f: to_string 
      |> List.map ~f: string2type))
    |> (Map.of_alist_exn (module Var2valMap)) 
 
(* json2traceback: function that makes json file traceback *)
(* used in json2tracebacks *)
  let json2traceback: Yojson.Basic.t -> traceback
  = fun json -> 
    let filename = json |> member "info" |> member "filename" |> to_string in 
    let classname = json |> member "info" |> member "classname" |> to_string in
    let classname = if Stdlib.String.contains classname '.' then Stdlib.String.split_on_char '.' classname |> List.hd_exn 
    else classname in 
    let funcname = json |> member "info" |> member "funcname" |> to_string in 
    let line = json |> member "info" |> member "line" |> to_int in
    let args = (json |> member "args" |> to_args) (Meta { filename = filename ; classname = classname ; funcname = funcname }) in 
    Traceback {filename = filename ; classname = classname ; funcname = funcname ; lineno = line ; args = args}
(* json2tracebacks: function that makes json file tracebacks(traceback list) *)
  let json2tracebacks : Yojson.Basic.t-> tracebacks
  = fun json -> to_list json |> List.map ~f: json2traceback


  (* let tracebacks2candvars : tracebacks -> candvars
  = fun tracebacks -> List.fold_left ~init:[] ~f:(fun acc (Traceback x) -> Map.keys x.args @ acc) tracebacks *)

  let dynamicAnal : tracebacks -> negType
  = fun tracebacks -> List.map ~f: (fun (Traceback x) -> Map.to_alist x.args) tracebacks 
    |> List.concat |> Map.of_alist_exn (module Var2valMap)
   
end 


(* ************************ *)
(* Module: Type Constraints *)
(* ************************ *)

module TCon = struct

  type tcon = Constraint of { expr: expr * expr ; meta: meta_data } 
  and constraints = tcon list



  let rec generate_tCon : NegType.tracebacks -> constraints
  = fun tracebacks -> 
      match tracebacks with 
      | [] -> []
      | Traceback tb :: tl -> let pgm = filename2pgm tb.filename in 
          generate_tCon_stmtS (module2tracebacks pgm tb.classname tb.funcname) (Meta {filename = tb.filename ; classname = tb.classname ; funcname = tb.funcname }) @ generate_tCon tl




  and module2tracebacks : stmt list -> identifier -> identifier -> stmt list 
  = fun pgm classname funcname -> 
      if String.equal classname funcname then  List.filter ~f:(fun a -> isFuncDef a funcname) pgm
      else 
        begin
          let classdefs =  
          List.filter ~f:(fun a -> isClassDef a classname) pgm
          |> List.map ~f:(function ClassDef (classdef) -> classdef.body | _ -> []) |> List.concat in 
          List.filter ~f:(fun a -> isFuncDef a funcname) classdefs 
        end 

  and isClassDef
    = fun stmt classname ->
      match stmt with 
      | ClassDef x -> if String.equal x.name classname then true else false 
      | _ -> false

  and isFuncDef
     = fun pgm funcname -> 
      match pgm with 
      | FunctionDef x -> if String.equal x.name funcname then true else false
      | _ -> false 


      
  (* generate_tCon_stmts: constraints generating function for statements *)
 and generate_tCon_stmtS : stmt list -> meta_data -> constraints
  = fun stmtS meta -> List.fold_left ~init:[] ~f: (fun acc x -> ((generate_tCon_stmt x meta) @ acc)) stmtS
  
  
  (* generate_tCon_stmt: function generating constraints for statement *)
  and generate_tCon_stmt : stmt -> meta_data -> constraints
  = fun stmt meta -> 
    match stmt with
    | FunctionDef x -> generate_tCon_stmtS x.body meta
    | AsyncFunctionDef x -> generate_tCon_stmtS x.body meta
    | ClassDef x -> generate_tCon_stmtS x.body meta
    | Return x -> 
      begin 
        match x.value with 
        | None -> []
        | Some x' -> generate_tCon_expr x' meta 
      end 
    | Delete _ -> []
    | Assign x -> List.map x.targets ~f:(fun expr -> Constraint { expr = (expr, x.value) ; meta = meta })
    | AugAssign x -> [Constraint { expr = (x.target, x.value) ; meta = meta }]
    | AnnAssign x -> [Constraint {expr = (x.target, x.annotation) ; meta = meta }]
    | For x -> generate_tCon_stmtS x.body meta 
    | AsyncFor x -> generate_tCon_stmtS x.body meta 
    | While x -> generate_tCon_stmtS x.body meta 
    | If x -> generate_tCon_stmtS x.body meta @ generate_tCon_stmtS x.orelse meta 
    | With x -> generate_tCon_stmtS x.body meta 
    | AsyncWith x ->  generate_tCon_stmtS x.body meta 
    | Match _ -> []
    | Raise _ -> []
    | Try x -> generate_tCon_stmtS x.body meta @ generate_tCon_stmtS x.orelse meta 
    | Assert _ -> []
    | Import _ -> []
    | ImportFrom _ -> []
    | Global _ -> []
    | Nonlocal _ -> []
    | Expr x -> generate_tCon_expr x.value meta
    | Pass _ -> []
    | Break _ -> []
    | Continue _ -> []

(* generate_tCon_expr: function generating constraints for expression *)
  and generate_tCon_expr : expr -> meta_data -> constraints
  = fun expr meta ->
      match expr with 
          | BoolOp x -> iter_boolop x.values meta 
          | NamedExpr x -> [Constraint { expr = (x.target, x.value) ; meta = meta }]
          | BinOp x -> [Constraint {expr = (x.left, x.right) ; meta = meta }]
          | UnaryOp _ -> []
          | Lambda _ -> []
          | IfExp _ -> []
          | Dict _ -> []
          | Set _ -> []
          | ListComp _ -> []
          | SetComp _ -> []
          | DictComp _ -> []
          | GeneratorExp _ -> []
          | Await _ -> []
          | Yield _ ->  []
          | YieldFrom _ -> []
          | Compare _ -> []
          | Call x -> List.fold_left ~init:[] ~f:(fun acc x -> (generate_tCon_expr x meta) @ acc) x.args
          | FormattedValue _ -> []
          | JoinedStr _ -> []
          | Constant _ -> [] 
          | Attribute _ -> []
          | Subscript _ -> []
          | Starred _ -> []
          | Name _ -> []
          | List _ -> []
          | Tuple _ -> [] 
          | Slice _ -> []

  

    and iter_boolop : expr list -> meta_data -> constraints
    = fun exprs meta ->
      match exprs with 
      | [] | _ :: [] -> []  
      | x :: y :: [] -> Constraint { expr = (x, y) ; meta = meta } :: []
      | x :: y :: tl ->  Constraint { expr = (x, y) ; meta = meta } :: iter_boolop (y :: tl) meta 

end 



(* ************************** *)
(* Print functions (for test) *)
(* ************************** *)

module Print = struct 
  let rec print_type : var_type list -> unit
  = fun lst -> 
     match lst with 
    | [] -> ()
    | hd :: tl  -> 
      begin 
        match hd with 
        | VInt -> Stdio.print_string "VInt " ; print_type tl
        | VFloat -> Stdio.print_string "VFlaot " ; print_type tl
        | VString -> Stdio.print_string "VString " ; print_type tl
        | VBool -> Stdio.print_string "VBool " ; print_type tl
        | VDict -> Stdio.print_string "VDict " ; print_type tl
        | VList -> Stdio.print_string "VList " ; print_type tl 
        | VMethod -> Stdio.print_string "VMethod " ; print_type tl
        | VClass x -> Stdio.print_string ("VClass (" ^ x ^ ")") ; print_type tl 
        | VNone -> Stdio.print_string "VNone " ; print_type tl 
        | VEllipsis -> Stdio.print_string "VEllipsis " ; print_type tl

      end


  let print_args 
  = fun args -> 
    let alist = Map.to_alist args in 
    List.fold_left ~init:() ~f: (fun _ (Var(key), value)-> Stdio.print_string key.name ; 
    Stdio.print_string (" as " ^ key.alias ^ " ")  ; 
    print_type value) alist

  
  let print_traceback : NegType.traceback -> unit
  = fun traceback ->
    match traceback with 
    | Traceback x -> 
      Stdio.print_endline "\nTraceback" ;
      Stdio.print_endline x.filename ;
      Stdio.print_endline x.classname ;
      Stdio.print_endline x.funcname ;
      Stdlib.Printf.printf "%d\n" x.lineno ;
      print_args x.args
      


  let print_tracebacks : NegType.tracebacks -> unit
  = fun tracebacks -> List.fold_left ~init:() ~f:(fun _ x -> print_traceback x) tracebacks


  end 





(* ************************ *)
(* Module: Type Environment *)
(* ************************ *)



module TEnv = struct
  type value = var_type list 
  type tEnv = value Map.M (Var2valMap).t

 (* Λ^{init} in PyTER *)
 (* init_tEnv: function initializing type envrionment for static analysis *)
  let rec init_tEnv : variable -> NegType.negType -> tEnv
  = fun var tenv -> Map.set tenv ~key: var ~data: []


  and print_map : tEnv -> unit
  = fun tenv ->
    let _ = Map.mapi tenv ~f:(fun ~key: (Var key) ~data: _ -> Stdio.print_string (key.name ^ " as " ^  key.alias ^ " ") ; 
    Print.print_type (Map.find_exn tenv (Var key)) ; Stdio.print_endline "") in ()



   (* Φ in PyTER *)  
   (* updating_tEnv: function that infers type of buggy variable with the information of constarints(i.e. constraints) *)
  and updating_tEnv : variable -> TCon.constraints -> tEnv -> tEnv
    = fun (Var var) tcon tenv -> 
      match tcon with 
      | [] -> tenv
      | Constraint(tcon) :: tl -> 
        (* if compVar var.meta tcon.meta then *)
        begin 
          match tcon.expr with 
          | Name x, Name y ->
              let () = Stdio.print_endline "\n*****updating1*****\n" ; print_map tenv in  (* Print for test *)
              if String.equal x.id var.name && String.equal y.id var.name then updating_tEnv (Var var) tl tenv
              else if String.equal x.id var.name then Map.update tenv (Var var) ~f:(fun values -> (Option.value ~default: [VEllipsis] values) @ findVal y.id tcon.meta tenv)
              |> updating_tEnv (Var var) tl 
              else if String.equal y.id var.name then Map.update tenv (Var var) ~f:(fun values -> (Option.value ~default: [VEllipsis] values) @ findVal x.id tcon.meta tenv)
              |> updating_tEnv (Var var) tl 
               else updating_tEnv (Var var) tl tenv 
          | Name x, y -> 
              let () = Stdio.print_endline "\n*****upating2*****\n" ; print_map tenv in (* Print for test *)
            if String.equal x.id var.name then (updating_Name_with__ y (Var var) tenv) |> updating_tEnv (Var var) tl 
          else updating_tEnv (Var var) tl tenv
          | x, Name y -> 
              let () = Stdio.print_endline "\n*****updating3*****\n"  ; print_map tenv in (* Print for test *)
            if String.equal y.id var.name then (updating_Name_with__ x (Var var) tenv) |> updating_tEnv (Var var) tl 
          else updating_tEnv (Var var) tl tenv
          | _ -> updating_tEnv (Var var) tl tenv
        end 
      (* else updating_tEnv (Var var) tl tenv *)

  and updating_Name_with__ : expr -> variable -> tEnv -> tEnv
  = fun expr var tenv ->
      match expr with 
      | Constant x -> Map.update tenv var ~f:(fun values -> (const2type x.value :: Option.value ~default:[] values))
      | Dict _ -> Map.update tenv var ~f:(fun values -> (VDict :: Option.value ~default:[] values))
      | List _ -> Map.update tenv var ~f:(fun values -> (VList :: Option.value ~default:[] values))
      | _ -> tenv

  and compVar : meta_data -> meta_data -> bool 
  = fun (Meta var) (Meta tcon) -> 
    (String.equal var.filename tcon.filename) && (String.equal var.classname tcon.classname) && (String.equal var.funcname tcon.funcname)
  and findVal : identifier -> meta_data -> tEnv-> value
  = fun name meta tenv -> 
    let tenv_list = Map.to_alist tenv in
    let (_, var)= List.find_exn tenv_list ~f:(fun ((Var var), _) -> compVar var.meta meta && String.equal var.name name) in var

(* dom_type: function that returns mode type in var_type list *)
  and dom_type : value -> value
  = fun values -> 
    (* let () = Print.print_type values in  *)
    let types = List.dedup_and_sort ~compare: (fun x y -> String.compare (type2string x) (type2string y)) values in 
    (* let () = Print.print_type types in *)
    let ordered = List.map ~f:(fun x -> (x, count_values x values)) types in
    let ordered = List.sort ~compare: (fun (_, a) (_, b) -> a-b) ordered  |> List.rev in 
    let max = match List.hd ordered with Some (_ ,num) -> num | None -> 0 in 
    List.filter ~f: (fun(_,a) -> a = max) ordered
    |> List.map ~f: (fun (a, _) -> a)

(* used in dom_type *)
(* count_values: function counting the number of certain type "value" *)
  and count_values : var_type -> value -> int
  = fun value values -> let is_type = is_type value in 
      List.fold_left ~init:0 ~f: (fun acc x -> is_type x + acc) values

(* used in count_values *)
(* is_type: function comparing value and target; if same return 1 else 0 *)
  and is_type : var_type -> var_type -> int 
  = fun value target -> if String.equal (type2string value) (type2string target) then 1 else 0
  
end   




let anal : Yojson.Basic.t -> TEnv.tEnv
= fun dynamic -> 
  let tracebacks = NegType.json2tracebacks dynamic in 
  let negtype = NegType.dynamicAnal tracebacks in
  let () = Stdio.print_endline "\n*****negtype*****\n" ; TEnv.print_map negtype in(* Print for test *)
  let constraints = TCon.generate_tCon tracebacks in
  let _ = Stdio.print_endline "\n*****constraints*****\n" ;  List.map ~f:(fun (Constraint x) -> (match x.expr with (a, b) -> (Stdio.print_endline ("(" ^ Pycaml.Ast2string.string_of_expr a ^ "," ^  Pycaml.Ast2string.string_of_expr b ^ ")")))) constraints in(* Print for test *)
  Map.mapi ~f: (fun ~key: var ~data: _ -> 
    (TEnv.init_tEnv var negtype
  |> TEnv.updating_tEnv var constraints
  |> Map.find_exn) var
  |> TEnv.dom_type) negtype