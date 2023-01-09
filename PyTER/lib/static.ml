open Ast
open Util
(* type val = 
| VInt of int
| VFloat of float 
| VString of string
| VBool of bool 
| V[]
| VEllips *)


(* Frame *)

(* 1. modulation json file of the Candvars from Dynamic_neg *)
(* 2. Convert Python file to AST file of OCaml *)
(* 3. Execute Static analysis: iteration*)
(*  3-1. Make a type environment: target one variable *)
(*  3-2. Infer type of the variable *)
(*  3-3. After the type inference, Compare result of 
         static type inference with the result of  dynamic analysis*)

(* module for result of dynamic analysis *)



let rec anal : CandVars.t -> constant list list
= fun candvars -> 
    let varInCandvars = List.map (fun (a, _) -> a) candvars in 
      _anal varInCandvars



and _anal : identifier list -> constant list list
= fun varInCandvars -> 
    match varInCandvars with 
    | [] -> [] 
    | var :: tl -> infer var (TEnv.init_tEnv var candvars) :: _anal tl

(* Module: Type Constraints*)
module TCon = struct
  type t = expr * expr list

end 
(* Module: Candidate Variables *)

module CandVars = struct
  type t = identifier * (constant list) list 
  (* json2list: convert the result of dynamic analysis to (identifier * (constant list) list) *)
  (* let rec json2list TODO *)

end 

(*
module TEnvMap = struct
  type t = Map.Make (constant list)

  let init_tEnv : identifier -> CandVars.t -> TEnvMap.t
  = fun var candvars -> Map.filter ~f:(fun v -> v <> var) candvars
end
*)



 (* Module: Type Environment *)
module TEnv = struct
  type t = identifier * (constant list) list

  let rec init_tEnv : identifier -> CandVars.t -> TEnv.t 
  = fun var candvars -> List.filter (fun (a, _) -> a <> var) candvars

  let rec infer: identifier -> TEnv.t -> modul -> constant list list
  = fun var tenv pgm ->  
      match pgm with 
      | Module mod -> let constraints = generate_TCon_stmtS mod.body in 
      | Expression exp -> let constraints = generate_TCon_expr exp.body in 
      | _ -> (* TODO *)



    and generate_TCon_expr: expr -> TCon.t
     fun expr -> 
       match exp.body with 
          (* | BoolOp of { op: boolop ; values: expr list ; attrs: attributes } *)
          (* and | or *)
          | BoolOp x -> TEnv.iter_boolop x.values
          (* | NamedExpr of { target: expr ; value: expr ; attrs: attributes } *)
          (* := *)
          | NamedExpr x -> [(x.target, x.value)]
          (* | BinOp of { left: expr ; op: operator ; right: expr ; attrs: attributes } *)
          | BinOp x -> [(x.left, x.right)]
          (* | UnaryOp of {op: unaryop ; operand: expr ; attrs: attributes } *)
          | UnaryOp x -> []
          (* | Lambda of { args: arguments ; body: expr ; attrs: attributes } *)
          | Lambda x -> []
          (* | IfExp of { test: expr; body:  expr; orelse: expr; attrs: attributes } *)
          | IfExp x -> []
          (* | Dict of { keys: (expr option) list ; values: expr list ; attrs: attributes } *)
          | Dict x -> []
          (* | Set of { elts: expr list ; attrs: attributes } *)
          | Set x -> []
          (* | ListComp of { elt: expr ; generators: comprehension list ; attrs: attributes } *)
          | ListComp x -> []
          (* | SetComp of { elt: expr ; generators: comprehension list ; attrs: attributes } *)
          | SetComp x -> []
          (* | DictComp of { key: expr ; value: expr ; generators: comprehension list ; attrs: attributes } *)
          | DictComp x -> []
          (* | GeneratorExp of { elt: expr ; generators: comprehension list ; attrs: attributes } *)
          | GeneratorExp x -> []
          (* | Await of { value: expr ; attrs: attributes } *)
          | Await x -> []
          (* | Yield of {value: expr option ; attrs: attributes } *)
          | Yield x ->  []
          (* | YieldFrom of { value: expr ; attrs: attributes }  *)
          | YieldFrom x -> []
          (* | Compare of { left: expr ; ops: cmpop list ; comparators: expr list ; attrs: attributes }  *)
          | Compare x -> []
          (* | Call of { func: expr ; args: expr list ; keywords: keyword list ; attrs: attributes } *)
          | Call x -> TEnv.iter_call x.args
          (* | FormattedValue of { value: expr ; conversion: int ; format_spec: expr option ; attrs: attributes } *)
          | FormattedValue x -> []
          (* | JoinedStr of { values: expr list ; attrs: attributes } *)
          | JoinedStr x -> []
          (* | Constant of { value: constant ; kind: string option ; attrs: attributes } *)
          | Constant x -> [] 
          (* | Attribute of { value: expr ; attr: identifier ; ctx: expr_context ; attrs: attributes } *)
          | Attribute x -> []
          (* | Subscript of { value: expr ; slice: expr ; ctx: expr_context ; attrs: attributes } *)
          | Subscript x -> []
          (* | Starred of { value: expr ; ctx: expr_context ; attrs: attributes } *)
          | Starred x -> []
          (* | Name of { id: identifier ; ctx: expr_context ; attrs: attributes } *)
          | Name x -> []
          (* | List of { elts: expr list ; ctx: expr_context ; attrs: attributes }  *)
          | List x -> []
          (* | Tuple of { elts: expr list ; ctx: expr_context ; attrs: attributes } *)
          | Tuple x -> [] 
          (* | Slice of { lower: expr option ; upper: expr option ; step: expr option ; attrs: attributes } *)
          | Slice x -> []
        end 

    
    and generate_TCon_stmtS : stmt list ->  TCon.t
     = fun stmts -> 
        match stmts with 
        | [] -> [] 
        | hd :: tl -> generate_TCon_stmt hd @ generate_TCon_stmtS tl 

    and generate_TCon_stmt : stmt -> TCon.t
      = fun stmt -> 
        match stmt with 
        (* | FunctionDef of { name: identifier ; args : arguments ; body: stmt list ; decorator_list: expr list ; returns: expr option ; type_comment: string option; attrs: attributes } *)
        | FunctionDef x -> 
          begin 
            match x.body with 
            | [] -> [] 
            | _ -> generate_TCon_stmtS x.body
          end 
        (* | AsyncFunctionDef of { name: identifier; args: arguments; body: stmt list ; decorator_list: expr list; returns: expr option ; type_comment: string option ; attrs: attributes}  *)
        | AsyncFunctionDef x -> 
          begin 
            match x.body with 
            | [] -> [] 
            | _ -> generate_TCon_stmtS x.body
          end 
        (* | ClassDef of { name: identifier ; bases : expr list ; keywords: keyword list; body: stmt list; decorator_list: expr list; attrs: attributes} *)
        | ClassDef x -> []
        (* | Return of { value: expr option ; attrs: attributes } *)
        | Return x -> 
          begin 
            match x.value with 
            | None -> []
            | Some x' -> generate_TCon_expr x'.value
          end 
        (* | Delete of { targets : expr list ; attrs:attributes } *)
        | Delete x -> []
        (* | Assign of { targets: expr list; value: expr ; type_comment: string option ; attrs: attributes} *)
        | Assignment x -> TEnv.iter_assign x.targets x.value
        (* | AugAssign of {target: expr ; op: operator ; value: expr ; attrs: attributes}  *)
        | AugAssign x -> (x.target, x.value)
        (* | AnnAssign of {target: expr; annotation : expr; value: expr option ; simple : int ; attrs: attributes} *)
        (* Annotation Assignment: Assignment with annotation *)
        | AnnAssign x -> (x.target, x.annotation)
        (* | For of { target: expr ; iter: expr ; body: stmt list ; orelse: stmt list; type_comment: string option ; attrs : attributes}  *)
        | For x -> 
          begin 
            match x.body with
            | [] -> [] 
            | _ -> generate_TCon_stmtS x.body
          end 
        (* | AsyncFor of { target: expr ; iter: expr ; body: stmt list ; orelse: stmt list; type_comment: string option ; attrs: attributes} *)
        | AsyncFor x -> 
          begin 
            match x.body with 
            | [] -> []
            | _ -> generate_TCon_stmtS x.body 
          end 
        (* | While of { test: expr; body: stmt list ; orelse: stmt list ; attrs: attributes } *)
        | While x ->
            begin 
              match x.body with 
              | [] -> []
              | _ -> generate_TCon_stmtS x.body
            end 
        (* | If of { test: expr ; body: stmt list ; orelse : stmt list ; attrs: attributes } *)
        | If x -> 
          begin 
            match x.body with
            | [] -> [] 
            | _ -> generate_TCon_stmtS x.body
          end 
        (* | With of { items: withitem list ; body: stmt list ; type_comment: string option ; attrs: attributes } *)
        | With x -> 
          begin 
            match x.body with 
            | [] -> []
            | _ -> generate_TCon_stmtS x.body
          end 
        (* | AsyncWith of { items: withitem list ; body: stmt list ; type_comment: string option ; attrs: attributes } *)
        | AsyncWith x -> 
          begin 
            match x.body with 
            | [] -> []
            | _ -> generate_TCon_stmtS x.body
          end 
        (* | Match of { subject: expr ; cases : match_case list ; attrs: attributes }  *)
        | Match x -> []
        (* | Raise of { exc: expr option ; cause : expr option ; attrs : attributes }  *)
        | Raise x -> [] 
        (* | Try of { body : stmt list ; handlers: excepthandler list ; orelse: stmt list ; finalbody: stmt list ; attrs: attributes } *)
        | Try x -> []
        (* | Assert of { test: expr ; msg: expr option ; attrs: attributes } *)
        | Assert x -> []
        (* | Import of { names: alias list; attrs : attributes }  *)
        | Import x -> []
        (* | ImportFrom of {modul: identifier option ; names: alias list ; level : int option ; attrs: attributes } *)
        | ImportFrom x -> []
        (* | Global of { names: identifier list ; attrs: attributes } *)
        | Global x -> []
        (* | Nonlocal of { names: identifier list ; attrs: attributes } *)
        | Nonlocal x -> [] 
        (* | Expr of { value : expr ; attrs: attributes} *)
        | Expr x -> generate_TCon_expr x.value
        (* | Pass of { attrs: attributes } *)
        | Pass x -> []
        (* | Break of { attrs: attributes }  *)
        | Break x -> []
        (* | Continue of { attrs: attributes } *)
        | Continiue x -> []


  
    and iter_boolop : expr list -> TCon.t
    = fun exprs -> 
      match exprs with 
      | [] -> [] 
      | a :: b :: tl -> (a, b) :: iter_boolop (b :: tl)
    
    and iter_assign : expr list -> expr -> TCon.t
    = fun exprs expr -> 
      match exprs with 
      | [] -> [] 
      | hd :: tl -> (hd, expr) :: iter_assign tl expr

    and iter_call : expr list -> expr list
      = fun exprs -> 
        match exprs with 
        | [] -> [] 
        | hd :: tl -> generate_TCon_expr hd @ iter_call tl

    and updating_tenv : TCon.t -> TEnv.t -> TEnv.t
      = fun tcon tenv -> 
        match tcon with 
        | [] -> tenv
        | hd :: tl -> 
          begin 
            match hd with 
            | Name x, Name y ->
            | Name x, Constant y -> 
            | Constant x, Name y -> 
            | _ -> updating_tenv tl tenv
          end 



    and lookup_tenv : TEnv.t -> constant list list
      = fun tenv -> 





