open Static
open Flocal
open Pycaml.Ast
open Base



(* type delta = Delta of { var : variable ; linenos: int list ; postype: posType ; negtype : negType } *)
module Templates = struct

  type templates = template list
  and template =
  | NegTypeCasting of stmt
  | PosTypeCasting of stmt 
  | TypeCastingExpr of stmt
  | NegHandlingStmt of stmt
  | NegHandlingExpr of stmt
  | PosHandling of stmt
  | ExceptionHandling of stmt
  | NegGuard of stmt
  | PosGuard of stmt


  let mapTemplates : delta -> (lineno * stmt) -> (lineno * templates) 
  = fun (Delta delta) (line, stmt) -> 
    match stmt with 
    (* Guard *)
    | If _ -> if Map.length delta.negtype = 1 then line, [NegGuard(stmt)] else line, [PosGuard(stmt)]
    | _ -> if Map.length delta.postype = 1 then 
    (* Type Casting *)
      begin 
        if Map.length delta.negtype = 1 then line, [NegTypeCasting(stmt) ; TypeCastingExpr(stmt)]
        else line, [PosTypeCasting(stmt) ; TypeCastingExpr(stmt)]
      end 
    (* Handling *)
      else 
      begin 
          if Map.length delta.negtype = 1 then line, [NegHandlingStmt(stmt) ; NegHandlingExpr(stmt) ; ExceptionHandling(stmt)]
          else line, [PosHandling(stmt) ; ExceptionHandling(stmt)]
      end 



  let stmt_of_linenos : variable -> linenos -> (lineno * stmt) list
  = fun (Var var) linenos -> 
    let Meta meta = var.meta in 
    let filename = meta.filename in
    let pgm = TCon.filename2pgm filename in 
    match pgm with 
    | Module x -> List.map linenos ~f:(fun line -> (line, List.find_exn x.body ~f:(fun stmt -> (SBFL.get_lineno stmt) = line)))
    | _ -> raise (Failure "not the program")



  let prioritizeTemplates : delta -> (lineno * templates) list
  = fun (Delta delta) -> 
    let target_stmts = stmt_of_linenos delta.var delta.linenos in
    List.map target_stmts ~f:(mapTemplates (Delta delta)) 









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