open Static
open Flocal
open Pycaml.Ast
open Base
open Pycaml



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
  type locNtplt = (lineno * template)
  type locNtplts  = (lineno * templates)
  type patch_set = delta * locNtplts

  let indent = 4

  let rec update_lineno : stmt list -> lineno -> int ->  unit 
  = fun pgm lineno num -> 
    match pgm with 
    | [] -> ()
    | hd :: tl -> if SBFL.get_lineno hd > lineno then
      begin increase_lineno hd num ; update_lineno tl lineno num end 
    else update_lineno tl lineno num 

  and increase_lineno 
  = fun stmt num -> 
    match stmt with
    | FunctionDef x -> x.attrs.lineno <- x.attrs.lineno + num 
    | AsyncFunctionDef x -> x.attrs.lineno <- x.attrs.lineno + num 
    | ClassDef x -> x.attrs.lineno <- x.attrs.lineno + num 
    | Return x -> x.attrs.lineno <- x.attrs.lineno + num 
    | Delete x -> x.attrs.lineno <- x.attrs.lineno + num 
    | Assign x -> x.attrs.lineno <- x.attrs.lineno + num 
    | AugAssign x -> x.attrs.lineno <- x.attrs.lineno + num 
    | AnnAssign x -> x.attrs.lineno <- x.attrs.lineno + num 
    | For x -> x.attrs.lineno <- x.attrs.lineno + num 
    | AsyncFor x -> x.attrs.lineno <- x.attrs.lineno + num 
    | While x -> x.attrs.lineno <- x.attrs.lineno + num 
    | If x -> x.attrs.lineno <- x.attrs.lineno + num 
    | With x -> x.attrs.lineno <- x.attrs.lineno + num 
    | AsyncWith x -> x.attrs.lineno <- x.attrs.lineno + num 
    | Match x -> x.attrs.lineno <- x.attrs.lineno + num 
    | Raise x -> x.attrs.lineno <- x.attrs.lineno + num 
    | Try x -> x.attrs.lineno <- x.attrs.lineno + num 
    | Assert x -> x.attrs.lineno <- x.attrs.lineno + num 
    | Import x -> x.attrs.lineno <- x.attrs.lineno + num 
    | ImportFrom x -> x.attrs.lineno <- x.attrs.lineno + num 
    | Global x -> x.attrs.lineno <- x.attrs.lineno + num 
    | Nonlocal x -> x.attrs.lineno <- x.attrs.lineno + num 
    | Expr x -> x.attrs.lineno <- x.attrs.lineno + num 
    | Pass x -> x.attrs.lineno <- x.attrs.lineno + num 
    | Break x -> x.attrs.lineno <- x.attrs.lineno + num 
    | Continue x -> x.attrs.lineno <- x.attrs.lineno + num 



  let rec insert_stmt : stmt list -> stmt -> lineno -> lineno ->  stmt list 
  = fun pgm stmt lineno col_offset -> 
    match pgm with 
    | [] -> [] 
    | hd :: tl -> if SBFL.get_lineno hd < lineno then hd :: stmt :: tl 
    else hd :: insert_stmt tl stmt lineno 
    
  let get_col_offset 
  = fun stmt -> 
    match stmt with  
    | FunctionDef x -> x.attrs.col_offset
    | AsyncFunctionDef x -> x.attrs.col_offset
    | ClassDef x -> x.attrs.col_offset
    | Return x -> x.attrs.col_offset
    | Delete x -> x.attrs.col_offset
    | Assign x -> x.attrs.col_offset
    | AugAssign x -> x.attrs.col_offset
    | AnnAssign x -> x.attrs.col_offset
    | For x -> x.attrs.col_offset
    | AsyncFor x -> x.attrs.col_offset
    | While x -> x.attrs.col_offset
    | If x -> x.attrs.col_offset
    | With x -> x.attrs.col_offset
    | AsyncWith x -> x.attrs.col_offset
    | Match x -> x.attrs.col_offset
    | Raise x -> x.attrs.col_offset
    | Try x -> x.attrs.col_offset
    | Assert x -> x.attrs.col_offset
    | Import x -> x.attrs.col_offset
    | ImportFrom x -> x.attrs.col_offset
    | Global x -> x.attrs.col_offset
    | Nonlocal x -> x.attrs.col_offset
    | Expr x -> x.attrs.col_offset
    | Pass x -> x.attrs.col_offset
    | Break x -> x.attrs.col_offset
    | Continue x -> x.attrs.col_offset

  let coordinate_line stmt lineno col_offset 
  = match stmt with
    | FunctionDef x -> x.attrs.lineno <- lineno ; x.attrs.col_offset <- col_offset 
    | AsyncFunctionDef x -> x.attrs.lineno <- lineno ; x.attrs.col_offset <- col_offset 
    | ClassDef x -> x.attrs.lineno <- lineno ; x.attrs.col_offset <- col_offset 
    | Return x -> x.attrs.lineno <- lineno ; x.attrs.col_offset <- col_offset 
    | Delete x -> x.attrs.lineno <- lineno ; x.attrs.col_offset <- col_offset 
    | Assign x -> x.attrs.lineno <- lineno ; x.attrs.col_offset <- col_offset 
    | AugAssign x -> x.attrs.lineno <- lineno ; x.attrs.col_offset <- col_offset 
    | AnnAssign x -> x.attrs.lineno <- lineno ; x.attrs.col_offset <- col_offset 
    | For x -> x.attrs.lineno <- lineno ; x.attrs.col_offset <- col_offset 
    | AsyncFor x -> x.attrs.lineno <- lineno ; x.attrs.col_offset <- col_offset 
    | While x -> x.attrs.lineno <- lineno ; x.attrs.col_offset <- col_offset 
    | If x -> x.attrs.lineno <- lineno ; x.attrs.col_offset <- col_offset 
    | With x -> x.attrs.lineno <- lineno ; x.attrs.col_offset <- col_offset 
    | AsyncWith x -> x.attrs.lineno <- lineno ; x.attrs.col_offset <- col_offset 
    | Match x -> x.attrs.lineno <- lineno ; x.attrs.col_offset <- col_offset 
    | Raise x -> x.attrs.lineno <- lineno ; x.attrs.col_offset <- col_offset 
    | Try x -> x.attrs.lineno <- lineno ; x.attrs.col_offset <- col_offset 
    | Assert x -> x.attrs.lineno <- lineno ; x.attrs.col_offset <- col_offset 
    | Import x -> x.attrs.lineno <- lineno ; x.attrs.col_offset <- col_offset 
    | ImportFrom x -> x.attrs.lineno <- lineno ; x.attrs.col_offset <- col_offset 
    | Global x -> x.attrs.lineno <- lineno ; x.attrs.col_offset <- col_offset 
    | Nonlocal x -> x.attrs.lineno <- lineno ; x.attrs.col_offset <- col_offset 
    | Expr x -> x.attrs.lineno <- lineno ; x.attrs.col_offset <- col_offset 
    | Pass x -> x.attrs.lineno <- lineno ; x.attrs.col_offset <- col_offset 
    | Break x -> x.attrs.lineno <- lineno ; x.attrs.col_offset <- col_offset 
    | Continue x -> x.attrs.lineno <- lineno ; x.attrs.col_offset <- col_offset 



  module TypeCasting = struct 


      
    let rec negTypeCasting_N_Test cast_holes code (Meta meta) =  
      let pgm = filename2pgm meta.filename in 
        match cast_holes with 
        | [] -> None 
        | hd :: tl -> let patch_code = code ^ hd in 
          let _ = Stdlib.Sys.command("echo " ^ patch_code ^ " >> /tmp/pycode.py | python3.10 pycaml/ast2json.py > /tmp/linecode.json" ) in
            let json = Yojson.Basic.from_file "/tmp/linecode.json" in 
              let modul = Json2ast.to_module json in 
              begin 
              match modul with 
              | Module x -> x.body |> List.hd_exn |> coordinate_line |> insert_code
              | _ -> raise (Failure "Undefined line") 
              end

          
    let negTypeCasting pgm (Delta delta, (lineno, NegTypeCasting stmt)) = 
      let Var var = delta.var in 
      let () = update_lineno pgm lineno 1 in
      let sus_var = var.name in  
      let neg_hole = Map.find_exn delta.negtype (Var var) |> List.hd_exn |> type2string in
      let cast_hole = Map.find_exn delta.postype (Var var) |> List.map ~f:(type2string) in
      let patch_code = "if isinstance (" ^ sus_var ^ ", " ^ neg_hole ^ ") : " ^ sus_var ^ " = " in
        negTypeCasting_N_Test cast_hole patch_code var.meta

      


 


        
    
      
      
  
    let posTypeCasting (Delta delta, lineno) = 
    let typeCastingExpr (Delta delta, lineno) = 



  end 

  module Handling = struct
    let negHandlingStmt (Delta delta, lineno) = 
    let negHandlingExpr (Delta delta, lineno) = 
    let posHandling (Delta delta, lineno) = 
    let exceptionHandling (Delta delta, lineno) = 


  end 

  module Guard = struct 
    let negGuard  (Delta delta, lineno) = 
    let posGuard (Delta delta, lineno) = 

  end 

  let dv_of_int  = 0 
  let dv_of_str = ""
  let dv_of_float = 0.0
  






  let mapTemplates : delta -> (lineno * stmt) -> locNtplts 
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



  let prioritizeTemplates : delta -> locNtplts list
  = fun (Delta delta) -> 
    let target_stmts = stmt_of_linenos delta.var delta.linenos in
    List.map target_stmts ~f:(mapTemplates (Delta delta)) 

  (* function tests: test whether the patched program passes test cases or not  *)
  let tests : modul -> bool 
  = fun modul -> 



  let patch_template
  = fun pgm (Delta delta, (lineno, tplt)) -> 
    let patched = 
    match tplt with 
    | NegTypeCasting x -> TypeCasting.negTypeCasting pgm delta lineno 
    | PosTypeCasting x -> TypeCasting.posTypeCasting pgm delta lineno 
    | TypeCastingExpr x -> TypeCasting.typeCastingExpr pgm delta lineno 
    | NegHandlingStmt x -> Handling.negHandlingStmt pgm delta lineno 
    | NegHandlingExpr x -> Handling.negHandlingExpr pgm delta lineno 
    | PosHandling x -> Handling.posHandling pgm delta lineno 
    | ExceptionHandling x -> Handling.exceptionHandling pgm delta lineno 
    | NegGuard x -> Guard.negGuard pgm delta lineno 
    | PosGuard x -> Guard.posGuard pgm delta lineno 
    in if tests patched then Module patched else None


  let rec patch_templates : stmt list -> patch_set -> pgm
  = fun pgm (Delta delta, (lineno, tplts)) -> 
    match tplts with 
    | [] -> None
    | hd :: tl -> 
      begin 
        match patch_template (Delta delta, (lineno, hd)) with 
        | Module x -> Module x
        | None -> patch_templates (Delta delta, (lineno, tl))
      end 






  let executePatch : delta list -> pgm list
  = fun dlist -> 
    List.map dlist ~f:(fun x -> (x, prioritizeTemplates x)) 
    |> List.map ~f:(patch_templates)

    


    

     

    









end 