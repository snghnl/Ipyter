open Ast
open Util
open Base



(* ************************ *)
(* Module: Type Constraints *)
(* ************************ *)
module TCon = struct
  type t = (expr * expr) list

end 




(* *************************** *)
(* Module: Candidate Variables *)
(* *************************** *)


module CandVars = struct

  type t = identifier * (constant list) list 
  (* json2list: convert the result of dynamic analysis to (identifier * (constant list) list) *)
  (* let rec json2list TODO *)

end 




(* **************************** *)
(* Module: Type Environment Map *)
(* **************************** *)

module TEnvMap = struct 
  type t = identifier
  type comparator_witness


end 
 





(* ************************ *)
(* Module: Type Environment *)
(* ************************ *)

module TEnv = struct
  type var = identifier
  type value = constant list
  type tEnv = value Map.M (TEnvMap).t

  let rec init_tEnv : var -> tEnv -> tEnv
  = fun var tenv -> Map.set tenv ~key:var ~data:[]

 and generate_tCon_stmtS : stmt list -> TCon.t
  = fun stmtS -> List.fold_left ~init:[] ~f: (fun acc x -> ((generate_tCon_stmt x) @ acc)) stmtS
  and generate_tCon_stmt : stmt -> TCon.t
  = fun stmt -> 
    match stmt with
    (* | FunctionDef of { name: identifier ; args : arguments ; body: stmt list ; decorator_list: expr list ; returns: expr option ; type_comment: string option; attrs: attributes } *)
    | FunctionDef x -> generate_tCon_stmtS x.body
    (* | AsyncFunctionDef of { name: identifier; args: arguments; body: stmt list ; decorator_list: expr list; returns: expr option ; type_comment: string option ; attrs: attributes}  *)
    | AsyncFunctionDef x -> generate_tCon_stmtS x.body
    (* | ClassDef of { name: identifier ; bases : expr list ; keywords: keyword list; body: stmt list; decorator_list: expr list; attrs: attributes} *)
    | ClassDef _ -> []
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


  and generate_tCon_expr : expr -> TCon.t
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






   (* and updating_tenv : TCon.t -> tEnv -> tEnv
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
          end  *)



    (* and lookup_tenv : tEnv -> constant list list
      = fun tenv ->  *)
    

    
  (* and infer : var -> TCon.t -> tEnv -> var * value list *)
  (* = fun var tcon tenv -> 
    match tcon with  *)
    (* | *)

end   

(* 
let rec anal : CandVars.t -> constant list list
= fun candvars -> 
    let varInCandvars = List.map (fun (a, _) -> a) candvars in 
      _anal varInCandvars



and _anal : identifier list -> constant list list
= fun varInCandvars -> 
    match varInCandvars with 
    | [] -> [] 
    | var :: tl -> TEnv.infer var (TEnv.init_tEnv var candvars) :: _anal tl *)


