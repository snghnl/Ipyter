open Ast

(* type val = 
| VInt of int
| VFloat of float 
| VString of string
| VBool of bool 
| VNone
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



(* Module: Type Equations *)
module TyEqns = struct
  type t = expr * expr list


(* Module: Candidate Variables *)
and CandVars = struct
  type t = identifier * (constant list) list 
  (* json2list: convert the result of dynamic analysis to (identifier * (constant list) list) *)
  let rec json2list (* TODO *)

end 

 (* Module: Type Environment *)
and TEnv = struct
  type t = identifier * (constant list) list

  let rec init_tEnv : identifier -> CandVars.t -> TEnv.t 
  = fun var candvars -> List.filter (fun (a, _) -> a <> var) candvars

  let rec infer: identifier -> TEnv.t -> modul -> constant list list
  = fun var tenv pgm ->  
      match pgm with 
      | Module mod -> infer_stmtS mod.body 
      | Expression exp -> infer_expr exp.body
      | _ -> (* TODO *)


    and infer_expr : expr -> TEnv.t option 
     fun expr -> 
       match exp.body with 
          (* | BoolOp of { op: boolop ; values: expr list ; attrs: attributes } *)
          (* and | or *)
          | BoolOp x -> Some 
          (* | NamedExpr of { target: expr ; value: expr ; attrs: attributes } *)
          (* := *)
          | NamedExpr x -> Some (infer_expr x.target, infer_expr x.value)
          (* | BinOp of { left: expr ; op: operator ; right: expr ; attrs: attributes } *)
          | BinOp x -> Some (infer_expr x.left, infer_expr x.right)
          (* | UnaryOp of {op: unaryop ; operand: expr ; attrs: attributes } *)
          | UnaryOp x -> infer_expr x.operand
          (* | Lambda of { args: arguments ; body: expr ; attrs: attributes } *)
          | Lambda x -> infer_expr x.body
          (* | IfExp of { test: expr; body:  expr; orelse: expr; attrs: attributes } *)
          | IfExp x -> None
          (* | Dict of { keys: (expr option) list ; values: expr list ; attrs: attributes } *)
          | Dict x -> None
          (* | Set of { elts: expr list ; attrs: attributes } *)
          | Set x -> None
          (* | ListComp of { elt: expr ; generators: comprehension list ; attrs: attributes } *)
          | ListComp x -> None
          (* | SetComp of { elt: expr ; generators: comprehension list ; attrs: attributes } *)
          | SetComp x -> None
          (* | DictComp of { key: expr ; value: expr ; generators: comprehension list ; attrs: attributes } *)
          | DictComp x -> None
          (* | GeneratorExp of { elt: expr ; generators: comprehension list ; attrs: attributes } *)
          | GeneratorExp x -> None
          (* | Await of { value: expr ; attrs: attributes } *)
          | Await x -> 
          (* | Yield of {value: expr option ; attrs: attributes } *)
          | Yield x -> 
          (* | YieldFrom of { value: expr ; attrs: attributes }  *)
          | YieldFrom x -> 
          (* | Compare of { left: expr ; ops: cmpop list ; comparators: expr list ; attrs: attributes }  *)
          | Compare x ->
          (* | Call of { func: expr ; args: expr list ; keywords: keyword list ; attrs: attributes } *)
          | Call x -> 
          (* | FormattedValue of { value: expr ; conversion: int ; format_spec: expr option ; attrs: attributes } *)
          | FormattedValue x -> 
          | JoinedStr of { values: expr list ; attrs: attributes }
          | Constant of { value: constant ; kind: string option ; attrs: attributes }
          | Attribute of { value: expr ; attr: identifier ; ctx: expr_context ; attrs: attributes }
          | Subscript of { value: expr ; slice: expr ; ctx: expr_context ; attrs: attributes }
          | Starred of { value: expr ; ctx: expr_context ; attrs: attributes }
          | Name of { id: identifier ; ctx: expr_context ; attrs: attributes }
          | List of { elts: expr list ; ctx: expr_context ; attrs: attributes } 
          | Tuple of { elts: expr list ; ctx: expr_context ; attrs: attributes }
          | Slice of { lower: expr option ; upper: expr option ; step: expr option ; attrs: attributes }
        end 

    
    and infer_stmtS : stmt list ->  TyEqns.t
     = fun stmts -> 
        match stmts with 
        | [] -> [] 
        | hd :: tl -> infer_stmt hd :: infer_stmtS tl 

    and infer_stmt : stmt -> TyEqns.t 
      = fun stmt -> 
        match stmt with 
        | FunctionDef of { name: identifier ; args : arguments ; body: stmt list ; decorator_list: expr list ; returns: expr option ; type_comment: string option; attrs: attributes }
        | AsyncFunctionDef of { name: identifier; args: arguments; body: stmt list ; decorator_list: expr list; returns: expr option ; type_comment: string option ; attrs: attributes} 
        | ClassDef of { name: identifier ; bases : expr list ; keywords: keyword list; body: stmt list; decorator_list: expr list; attrs: attributes}
        | Return of { value: expr option ; attrs: attributes }
        | Delete of { targets : expr list ; attrs:attributes }
        | Assign of { targets: expr list; value: expr ; type_comment: string option ; attrs: attributes}
        | AugAssign of {target: expr ; op: operator ; value: expr ; attrs: attributes} 
        | AnnAssign of {target: expr; annotation : expr; value: expr option ; simple : int ; attrs: attributes}
        | For of { target: expr ; iter: expr ; body: stmt list ; orelse: stmt list; type_comment: string option ; attrs : attributes} 
        | AsyncFor of { target: expr ; iter: expr ; body: stmt list ; orelse: stmt list; type_comment: string option ; attrs: attributes}
        | While of { test: expr; body: stmt list ; orelse: stmt list ; attrs: attributes }
        | If of { test: expr ; body: stmt list ; orelse : stmt list ; attrs: attributes }
        | With of { items: withitem list ; body: stmt list ; type_comment: string option ; attrs: attributes }
        | AsyncWith of { items: withitem list ; body: stmt list ; type_comment: string option ; attrs: attributes }
        | Match of { subject: expr ; cases : match_case list ; attrs: attributes } 
        | Raise of { exc: expr option ; cause : expr option ; attrs : attributes } 
        | Try of { body : stmt list ; handlers: excepthandler list ; orelse: stmt list ; finalbody: stmt list ; attrs: attributes }
        | Assert of { test: expr ; msg: expr option ; attrs: attributes }
        | Import of { names: alias list; attrs : attributes }
        | ImportFrom of {modul: identifier option ; names: alias list ; level : int option ; attrs: attributes }
        | Global of { names: identifier list ; attrs: attributes }
        | Nonlocal of { names: identifier list ; attrs: attributes }
        | Expr of { value : expr ; attrs: attributes}
        | Pass of { attrs: attributes }
        | Break of { attrs: attributes } 
        | Continue of { attrs: attributes }




  let rec dom_type: constant * int list -> constant list list
  = fun 


end 