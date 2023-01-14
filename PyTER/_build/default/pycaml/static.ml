open Ast
open Base
open Yojson.Basic.Util 
open Util 


type pgm = modul 

type var_type = 
| VInt
| VFloat
| VString 
| VBool
| VNone
| VEllipsis 

(* const2type: function that convert constant type to var_type *)
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
  | _ -> VNone


(* **************************** *)
(* Module: Type Environment Map *)
(* **************************** *)


module Var2valMap = struct 
  module T = struct
    type t = identifier
    let compare t1 t2 = 
      String.compare t1 t2
    let sexp_of_t t : Sexp.t = Atom t
  end

  include T
  include Comparator.Make(T)

end 




(* *************** *)
(* Module: Dynamic *)
(* *************** *)


module Dynamic = struct
  type var = identifier
  type candVars = var list
  type value = var_type list
  type posType = value Map.M (Var2valMap).t
  type traceback = Traceback of {filename: string ; classname: identifier ; funcname: identifier ; line: int ; args: posType} 
  type tracebacks = traceback list
 
  

   let to_args : Yojson.Basic.t -> posType 
  = fun json -> 
    to_assoc json |> List.map ~f: (fun (a, b) -> (a, to_list b |> List.map ~f:to_string |> List.map ~f: string2type))
    |> List.filter ~f: (fun (_, b) -> not(List.mem b VNone ~equal: (fun a b -> phys_equal a b)))
    |> (Map.of_alist_exn (module Var2valMap)) 
 
(* json2traceback: function that makes json file traceback *)
(* used in json2tracebacks *)
  let json2traceback: Yojson.Basic.t -> traceback
  = fun json -> 
    let filename = json |> member "info" |> member "filename" |> to_string in 
    let classname = json |> member "info" |> member "classname" |> to_string in 
    let funcname = json |> member "info" |> member "funcname" |> to_string in 
    let line = json |> member "info" |> member "line" |> to_int in
    let args = json |> member "args" |> to_args in 
    Traceback {filename = filename ; classname = classname ; funcname = funcname ; line = line ; args = args}
(* json2tracebacks: function that makes json file tracebacks(traceback list) *)
  let json2tracebacks : Yojson.Basic.t-> tracebacks
  = fun json -> to_list json |> List.map ~f: json2traceback

  let tracebacks2candVars : tracebacks -> candVars
  = fun tracebacks -> List.fold_left ~init:[] ~f:(fun acc (Traceback x) -> Map.keys x.args @ acc) tracebacks


  let dynamicAnal : tracebacks -> posType
  = fun tracebacks -> List.map ~f: (fun (Traceback x) -> x.args) tracebacks 
    |> List.fold ~init: (Map.empty (module Var2valMap)) ~f: (fun acc x -> 
      match Map.append ~lower_part: x ~upper_part: acc with 
      | `Ok x -> x 
      | _ -> Map.empty (module Var2valMap)
    )


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
        | VNone -> Stdio.print_string "VNone " ; print_type tl 
        | VEllipsis -> Stdio.print_string "VEllipsis " ; print_type tl
      end


  let print_args : Dynamic.posType -> unit 
  = fun args -> 
    let alist = Map.to_alist args in 
    List.fold_left ~init:() ~f: (fun _ (key, value)-> Stdio.print_string key ; Stdio.print_string " " ; print_type value) alist ; Stdio.print_endline ""



  let print_traceback : Dynamic.traceback -> unit
  = fun traceback ->
    match traceback with 
    | Traceback x -> 
      Stdio.print_endline "\nTraceback" ;
      Stdio.print_endline x.filename ;
      Stdio.print_endline x.classname ;
      Stdio.print_endline x.funcname ;
      Stdlib.Printf.printf "%d\n" x.line ;
      print_args x.args
      


  let print_tracebacks : Dynamic.tracebacks -> unit
  = fun tracebacks -> List.fold_left ~init:() ~f:(fun _ x -> print_traceback x) tracebacks


end



(* ************************ *)
(* Module: Type Environment *)
(* ************************ *)



module TEnv = struct
  type var = identifier
  type value = var_type list 
  type tEnv = value Map.M (Var2valMap).t
  type constraints = (expr * expr) list

 (* Λ^{init} in PyTER *)
 (* init_tEnv: function initializing type envrionment for static analysis *)
  let rec init_tEnv : var -> Dynamic.posType -> tEnv
  = fun var tenv -> Map.set tenv ~key:var ~data: []

  and generate_tCon : pgm -> constraints
  = fun pgm -> 
    match pgm with 
    | Module x -> generate_tCon_stmtS x.body
    | Expression x -> generate_tCon_expr x.body
    | _ -> raise (Failure "not the program" )


  (* generate_tCon_stmts: constraints generating function for statements *)
 and generate_tCon_stmtS : stmt list -> constraints
  = fun stmtS -> List.fold_left ~init:[] ~f: (fun acc x -> ((generate_tCon_stmt x) @ acc)) stmtS
  
  
  (* generate_tCon_stmt: function generating constraints for statement *)
  and generate_tCon_stmt : stmt -> constraints
  = fun stmt -> 
    match stmt with
    (* | FunctionDef of { name: identifier ; args : arguments ; body: stmt list ; decorator_list: expr list ; returns: expr option ; type_comment: string option; attrs: attributes } *)
    | FunctionDef x -> generate_tCon_stmtS x.body
    (* | AsyncFunctionDef of { name: identifier; args: arguments; body: stmt list ; decorator_list: expr list; returns: expr option ; type_comment: string option ; attrs: attributes}  *)
    | AsyncFunctionDef x -> generate_tCon_stmtS x.body
    (* | ClassDef of { name: identifier ; bases : expr list ; keywords: keyword list; body: stmt list; decorator_list: expr list; attrs: attributes} *)
    | ClassDef x -> generate_tCon_stmtS x.body
    (* | Return of { value: expr option ; attrs: attributes } *)
    | Return x -> 
      begin 
        match x.value with 
        | None -> []
        | Some x' -> generate_tCon_expr x'
      end 
    (* | Delete of { targets : expr list ; attrs:attributes } *)
    | Delete _ -> []
    (* | Assign of { targets: expr list; value: expr ; type_comment: string option ; attrs: attributes} *)
    | Assign x -> List.map x.targets ~f:(fun expr -> (expr, x.value))
    (* | AugAssign of {target: expr ; op: operator ; value: expr ; attrs: attributes}  *)
    | AugAssign x -> [(x.target, x.value)]
    (* | AnnAssign of {target: expr; annotation : expr; value: expr option ; simple : int ; attrs: attributes} *)
    | AnnAssign x -> [(x.target, x.annotation)]
    (* | For of { target: expr ; iter: expr ; body: stmt list ; orelse: stmt list; type_comment: string option ; attrs : attributes}  *)
    | For x -> generate_tCon_stmtS x.body
    (* | AsyncFor of { target: expr ; iter: expr ; body: stmt list ; orelse: stmt list; type_comment: string option ; attrs: attributes} *)
    | AsyncFor x -> generate_tCon_stmtS x.body
    (* | While of { test: expr; body: stmt list ; orelse: stmt list ; attrs: attributes } *)
    | While x -> generate_tCon_stmtS x.body
    (* | If of { test: expr ; body: stmt list ; orelse : stmt list ; attrs: attributes } *)
    | If x -> generate_tCon_stmtS x.body @ generate_tCon_stmtS x.orelse
    (* | With of { items: withitem list ; body: stmt list ; type_comment: string option ; attrs: attributes } *)
    | With x -> generate_tCon_stmtS x.body
    (* | AsyncWith of { items: withitem list ; body: stmt list ; type_comment: string option ; attrs: attributes } *)
    | AsyncWith x ->  generate_tCon_stmtS x.body
    (* | Match of { subject: expr ; cases : match_case list ; attrs: attributes }  *)
    | Match _ -> []
    (* | Raise of { exc: expr option ; cause : expr option ; attrs : attributes }  *)
    | Raise _ -> []
    (* | Try of { body : stmt list ; handlers: excepthandler list ; orelse: stmt list ; finalbody: stmt list ; attrs: attributes } *)
    | Try x -> generate_tCon_stmtS x.body @ generate_tCon_stmtS x.orelse
     (* | Assert of { test: expr ; msg: expr option ; attrs: attributes } *)
    | Assert _ -> []
    (* | Import of { names: alias list; attrs : attributes } *)
    | Import _ -> []
    (* | ImportFrom of {modul: identifier option ; names: alias list ; level : int option ; attrs: attributes } *)
    | ImportFrom _ -> []
    (* | Global of { names: identifier list ; attrs: attributes } *)
    | Global _ -> []
    (* | Nonlocal of { names: identifier list ; attrs: attributes } *)
    | Nonlocal _ -> []
    (* | Expr of { value : expr ; attrs: attributes} *)
    | Expr x -> generate_tCon_expr x.value
    (* | Pass of { attrs: attributes } *)
    | Pass _ -> []
    (* | Break of { attrs: attributes }  *)
    | Break _ -> []
    (* | Continue of { attrs: attributes } *)
    | Continue _ -> []

(* generate_tCon_expr: function generating constraints for expression *)
  and generate_tCon_expr : expr -> constraints
  = fun expr ->
      match expr with 
          (* | BoolOp of { op: boolop ; values: expr list ; attrs: attributes } *)
          (* and | or *)
          | BoolOp x -> iter_boolop x.values
          (* | NamedExpr of { target: expr ; value: expr ; attrs: attributes } *)
          (* := *)
          | NamedExpr x -> [(x.target, x.value)]
        (* | BinOp of { left: expr ; op: operator ; right: expr ; attrs: attributes } *)
          | BinOp x -> [(x.left, x.right)]
          (* | UnaryOp of {op: unaryop ; operand: expr ; attrs: attributes } *)
          | UnaryOp _ -> []
          (* | Lambda of { args: arguments ; body: expr ; attrs: attributes } *)
          | Lambda _ -> []
          (* | IfExp of { test: expr; body:  expr; orelse: expr; attrs: attributes } *)
          | IfExp _ -> []
          (* | Dict of { keys: (expr option) list ; values: expr list ; attrs: attributes } *)
          | Dict _ -> []
          (* | Set of { elts: expr list ; attrs: attributes } *)
          | Set _ -> []
          (* | ListComp of { elt: expr ; generators: comprehension list ; attrs: attributes } *)
          | ListComp _ -> []
          (* | SetComp of { elt: expr ; generators: comprehension list ; attrs: attributes } *)
          | SetComp _ -> []
          (* | DictComp of { key: expr ; value: expr ; generators: comprehension list ; attrs: attributes } *)
          | DictComp _ -> []
          (* | GeneratorExp of { elt: expr ; generators: comprehension list ; attrs: attributes } *)
          | GeneratorExp _ -> []
          (* | Await of { value: expr ; attrs: attributes } *)
          | Await _ -> []
          (* | Yield of {value: expr option ; attrs: attributes } *)
          | Yield _ ->  []
          (* | YieldFrom of { value: expr ; attrs: attributes }  *)
          | YieldFrom _ -> []
          (* | Compare of { left: expr ; ops: cmpop list ; comparators: expr list ; attrs: attributes }  *)
          | Compare _ -> []
          (* | Call of { func: expr ; args: expr list ; keywords: keyword list ; attrs: attributes } *)
          | Call x -> List.fold_left ~init:[] ~f:(fun acc x -> (generate_tCon_expr x) @ acc) x.args
          (* | FormattedValue of { value: expr ; conversion: int ; format_spec: expr option ; attrs: attributes } *)
          | FormattedValue _ -> []
          (* | JoinedStr of { values: expr list ; attrs: attributes } *)
          | JoinedStr _ -> []
          (* | Constant of { value: constant ; kind: string option ; attrs: attributes } *)
          | Constant _ -> [] 
          (* | Attribute of { value: expr ; attr: identifier ; ctx: expr_context ; attrs: attributes } *)
          | Attribute _ -> []
          (* | Subscript of { value: expr ; slice: expr ; ctx: expr_context ; attrs: attributes } *)
          | Subscript _ -> []
          (* | Starred of { value: expr ; ctx: expr_context ; attrs: attributes } *)
          | Starred _ -> []
          (* | Name of { id: identifier ; ctx: expr_context ; attrs: attributes } *)
          | Name _ -> []
          (* | List of { elts: expr list ; ctx: expr_context ; attrs: attributes }  *)
          | List _ -> []
          (* | Tuple of { elts: expr list ; ctx: expr_context ; attrs: attributes } *)
          | Tuple _ -> [] 
          (* | Slice of { lower: expr option ; upper: expr option ; step: expr option ; attrs: attributes } *)
          | Slice _ -> []

  

    and iter_boolop : expr list -> (expr * expr) list
    = function 
      | [] | _ :: [] -> []  
      | x :: y :: [] -> (x, y) :: []
      | x :: y :: tl ->  (x, y) :: iter_boolop (y :: tl)






   (* Φ in PyTER *)  
   (* updating_tEnv: function that infers type of buggy variable with the information of constarints(i.e. constraints) *)
   and updating_tEnv : var -> constraints -> tEnv -> tEnv
      = fun var tcon tenv -> 
        match tcon with 
        | [] -> tenv
        | hd :: tl -> 
          begin 
            match hd with 
            | Name x, Name y ->
               if String.equal x.id var && String.equal y.id var then updating_tEnv var tl tenv
               else if String.equal x.id var then Map.update tenv var ~f:(fun values -> Option.value ~default: [] values @ Map.find_exn tenv x.id)
               else if String.equal y.id var then Map.update tenv var ~f:(fun values -> Option.value ~default: [] values @ Map.find_exn tenv y.id)
               else updating_tEnv var tl tenv
            | Name x, Constant y -> 
              if String.equal x.id var then Map.update tenv var ~f:(fun values -> (const2type y.value :: Option.value ~default:[] values))
              else updating_tEnv var tl tenv 
            | Constant x, Name y -> 
              if String.equal y.id var then Map.update tenv var ~f:(fun values -> (const2type x.value :: Option.value ~default:[] values))
              else updating_tEnv var tl tenv
            | _ -> updating_tEnv var tl tenv
          end 



(* dom_type: function that returns mode type in var_type list *)
  and dom_type : value -> value
  = fun values -> 
    let types = [VInt ; VBool ; VString ; VFloat] in let ordered = List.map ~f:(fun x -> (x, count_values x values)) types
    |> List.sort ~compare: (fun (_, a) (_, b) -> a-b) in let max = match List.hd ordered with Some (_ ,num) -> num | None -> -1 in List.filter ~f: (fun(_,a) -> a = max) ordered
    |> List.map ~f: (fun (a, _) -> a)


(* used in dom_type *)
(* count_values: function counting the number of certain type "value" *)
  and count_values : var_type -> value -> int
  = fun value values -> let is_type = is_type value in List.fold_left ~init:0 ~f: (fun acc x -> is_type x + acc) values

(* used in count_values *)
(* is_type: function comparing value and target; if same return 1 else 0 *)
  and is_type : var_type -> var_type -> int 
  = fun value target -> if phys_equal value target then 1 else 0
  
    
end   


let anal: pgm -> Dynamic.posType -> TEnv.tEnv
= fun program pos -> Map.mapi pos ~f: (fun ~key:key ~data:_ -> ((TEnv.updating_tEnv key (TEnv.generate_tCon program) (TEnv.init_tEnv key pos))
|> Map.find_exn) key |> TEnv.dom_type)