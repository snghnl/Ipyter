(* Patch: Patch Generalization *)
(* Input: suspicious function F, suspicous line l, suspicious variable x, the result of static anal_(PosType), dynamic anal_(NegType) *)
(* 1. Prioritizaing templates, using the information from PosType and NegType *)
(* 2. Initializing templates and tests whether repaired one passes all the test cases *)
(* 3. If passing all the test cases, return patched program *)
(* Output: Patched program *)
open Static
open Flocal
open Pycaml.Ast
open Base







(* type delta = Delta of { var : variable ; linenos: int list ; postype: posType ; negtype : negType } *)


module Templates = struct

type template = 
  | NegTypeCasting of stmt
  | PosTypeCasting of stmt
  | TypeCastringExpr of stmt
  | NegHandlingStmt of stmt
  | NegHandlingExpr of stmt
  | PosHandling of stmt
  | ExceptionHandling of stmt
  | NegGuard  of stmt
  | PosGuard of stmt



  let prioritizeTemplates : delta -> template list
  = fun (Delta delta) -> 
    let target_stmts = stmt_of_linenos delta.var delta.linenos in 
    

    and 






  and stmt_of_linenos : variable -> liennos -> (lineno * stmt) list
  = fun (Var var) linenos -> 
    let Meta meta = var.meta in 
    let filename = meta.filename in
    let pgm = TCon.filename2pgm filename in 
    match pgm with 
    | Module x -> List.map linenos ~f:(fun line -> (line, List.find_exn x.body ~f:(fun stmt -> (SBFL.gent_lineno stmt) = line)))
    | _ -> raise (Failure "not the program")




  module TypeCasting = struct 
    let NegTypeCasting = 
    let PosTypeCasting
    let TypeCastringExpr



  end 

  module Handling = struct
    let NegHandlingStmt
    let NegHandlingExpr
    let PosHandling
    let ExceptionHandling


  end 

  module Guard = struct 
    let NegGuard 
    let PosGuard

  end 
end 