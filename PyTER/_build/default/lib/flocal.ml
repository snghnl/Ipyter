open Static
open Pycaml.Ast
open Base
open Yojson.Basic.Util
(* open Util *)


(* Flocal: Fault Localization *)
(* Input: The result of static anal, dynamic anal *)
(* 1. select suspicious variable by comparing static anal_ with dynamic anal_ *)
(* 2. select suspicious function whose variable is suspicious variable *)
(* 3. select suspicious line in the function by SBFL(Spectrum Based Fault Localization) *)
(* Output: suspicious function F, suspicous line l, suspicious variable x, the result of static anal_(PosType), dynamic anal_(NegType) *)



type posType = Dynamic.posType
type negType = TEnv.tEnv
type lineno = int 
type linenos = lineno list

(* (F, x, l, PostType, NegType) *)
type delta = Delta of { var : variable ; linenos: linenos ; postype: posType ; negtype : negType }




module VarSet = struct 
  module T = struct 
    type t = var_type 
    let compare t1 t2 = 
      String.compare (type2string t1) (type2string t2)
    let sexp_of_t t :Sexp.t = Atom (type2string t)
  end 

  include T
  include Comparator.Make(T)
  end 




(* *************** *)
(* Module: FuncLev *)
(* *************** *)

module FuncLev = struct
  
    type value = var_type list
    type dynamic = value Map.M (Var2valMap).t
    type static = value Map.M (Var2valMap).t
    type varSet = Set.M(VarSet).t

    (* find_susVar: function finding suspicious variable *)
    let find_susFun : posType -> negType -> variable list 
    = fun dynamic static -> 
        let keys = Map.keys static in 
        let assoc = List.map ~f:(fun var ->
          let dynamic_var = (Map.find_exn dynamic var |> Set.of_list (module VarSet)) in
          let static_var = (Map.find_exn static var |> Set.of_list (module VarSet)) in
          let diff = (Set.union (Set.diff static_var dynamic_var) (Set.diff dynamic_var static_var) 
          |> Set.length) in (var, diff)) keys in
          let ordered = List.sort ~compare: (fun (_,a) (_,b) -> a-b) assoc |> List.rev in 

          let max = match List.hd ordered with Some (_ ,num) -> num | None -> -1 in 
          List.filter ~f: (fun(_,a) -> a = max) ordered
          |> List.map ~f: (fun (a,_) -> a)

end




(* ************************************************ *)
(* Module PosCase: get traces of Positive TestCases *)
(* ************************************************ *)


module PosCase = struct
  type posTraces = posTrace list 
  and posTrace = PosTrace of { filename: identifier ; lineno: int }
  let json2traces : Yojson.Basic.t -> variable -> posTraces 
  = fun json (Var var) ->
    let Meta meta = var.meta in 
    member meta.filename json |> keys 
    |> List.map ~f:(fun x -> PosTrace { filename = meta.filename ; lineno = Stdlib.int_of_string x }) 
    

    



end



(* ************ *)
(* Module: SBFL *)
(* ************ *)
(* find suspicious line *)

module SBFL = struct

  type lineno = int
  type option = string
  type poslines = lineno list
  type neglines = lineno list
  type weight = float
  type weighted_path = (lineno * weight) list

  let init_value = 0.0


(* Function init_lines: initialize lines for SBFL *)
  let rec init_lines : variable -> weighted_path
  = fun var -> 
    let func = load_func var in 
    List.map func ~f:(fun stmt -> (get_lineno stmt, init_value))

  (* Function load_func: load function to module in OCaml format *)
  and load_func : variable -> stmt list
  = fun (Var var) -> let (Meta meta) = var.meta in 
  let pgm = TCon.filename2pgm meta.filename in
  TCon.module2tracebacks pgm meta.classname meta.funcname

    
(* return line nubmer from statement *)
  and get_lineno : stmt -> lineno 
  = fun stmt -> 
    match stmt with
    | FunctionDef x -> x.attrs.lineno
    | AsyncFunctionDef x -> x.attrs.lineno
    | ClassDef x -> x.attrs.lineno
    | Return x -> x.attrs.lineno
    | Delete x -> x.attrs.lineno
    | Assign x -> x.attrs.lineno
    | AugAssign x -> x.attrs.lineno
    | AnnAssign x -> x.attrs.lineno
    | For x -> x.attrs.lineno
    | AsyncFor x -> x.attrs.lineno
    | While x -> x.attrs.lineno
    | If x -> x.attrs.lineno
    | With x -> x.attrs.lineno
    | AsyncWith x -> x.attrs.lineno
    | Match x -> x.attrs.lineno
    | Raise x -> x.attrs.lineno
    | Try x -> x.attrs.lineno
    | Assert x -> x.attrs.lineno
    | Import x -> x.attrs.lineno
    | ImportFrom x -> x.attrs.lineno
    | Global x -> x.attrs.lineno
    | Nonlocal x -> x.attrs.lineno
    | Expr x -> x.attrs.lineno
    | Pass x -> x.attrs.lineno
    | Break x -> x.attrs.lineno
    | Continue x -> x.attrs.lineno


  let linesNeg : Dynamic.tracebacks -> neglines
  = fun tracebacks -> 
    List.map ~f: (fun (Traceback x)-> x.lineno) tracebacks


  let linesPos : PosCase.posTraces -> poslines
  = fun poscase -> 
    List.map ~f: (fun (PosTrace x) -> x.lineno) poscase


  let rec get_susline : weighted_path -> neglines -> poslines -> lineno list
  = fun wpath neg pos -> 
    let wpath = List.fold_left ~init:wpath ~f:(fun acc x -> (update_weight acc x "neg")) neg in
    let lines = List.fold_left ~init:wpath ~f:(fun acc x -> (update_weight acc x "pos")) pos 
    |> List.sort ~compare:(fun (_, a) (_, b) -> Float.compare a b) |> List.rev in 
    let (_, max) = List.hd_exn lines in List.filter ~f:(fun (_, b) -> Float.equal b max) lines |> List.map ~f:(fun (a, _) -> a) 


  and update_weight: weighted_path -> lineno -> option -> weighted_path
  = fun wpath lineno option -> 
    match option with 
    | "pos" -> let target, weight = List.find_exn wpath ~f:(fun (target, _) -> target = lineno) in
    List.filter wpath ~f:(fun (line ,_) -> line <> target) |> List.cons (target, weight *. 0.01) 
    | "neg" -> let target, _ = List.find_exn wpath ~f:(fun (target, _) -> target = lineno) in
    List.filter wpath ~f:(fun (line, _) -> line <> target) |> List.cons (target, 1.0)
    | _ -> raise (Failure "Wrong option error")



end 


type posSet = PosCase.posTraces * posType
type negSet = Dynamic.tracebacks * negType

  let flocal : posSet -> negSet -> delta list
  = fun (pTraces, pType) (nTraces, nType) -> 
    let susVars = FuncLev.find_susFun pType nType in 
    List.map susVars ~f: (fun var -> let lines = SBFL.init_lines var in let line = SBFL.get_susline lines (SBFL.linesNeg nTraces) (SBFL.linesPos pTraces) in
    Delta { var = var ; linenos = line ; postype = pType ; negtype = nType })









