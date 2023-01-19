open Static
(* open Ast *)
open Base
(* open Util *)


(* Flocal: Fault Localization *)
(* Input: The result of static anal, dynamic anal *)
(* 1. select suspicious variable by comparing static anal_ with dynamic anal_ *)
(* 2. select suspicious function whose variable is suspicious variable *)
(* 3. select suspicious line in the function by SBFL(Spectrum Based Fault Localization) *)
(* Output: suspicious function F, suspicous line l, suspicious variable x, the result of static anal_(PosType), dynamic anal_(NegType) *)






type delta = Delta of { traceback: Dynamic.traceback ; line: int ; postype: Dynamic.posType ; negtype : TEnv.tEnv }



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
  
    type var = variable 
    type value = var_type list
    type dynamic = value Map.M (Var2valMap).t
    type static = value Map.M (Var2valMap).t
    type varSet = Set.M(VarSet).t

    (* find_susVar: function finding suspicious variable *)
    let find_susVar : Dynamic.posType -> TEnv.tEnv -> var list 
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

    
    let find_susFunc : var -> Dynamic.tracebacks -> Dynamic.tracebacks
    = fun var tracebacks -> List.filter ~f:(fun (Traceback x) -> 
        match Map.find x.args var with 
        | Some _ -> true
        | None -> false 
        ) tracebacks 


end






(* ************ *)
(* Module: SBFL *)
(* ************ *)

module SBFL = struct

type line_num = int
type weight = float
type weighted_path = (line_num * weight) list










end
