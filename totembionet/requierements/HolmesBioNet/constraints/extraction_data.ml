(************************************************************************)
(*** Extraction of Influence Graph and Hoare Triple of the input file ***)
(************************************************************************)

open Types;;
open Parse;;

(* We define the main variables of the influence graph and the path of the 
Hoare triple using the parser of the input file. *)
let (vars, mults) = identify_ig !input_data ;;
let (prog0, (d0, first_h0)) = identify_ht !input_data ;;
let isCyclic = identify_cyclic_behaviour !input_data ;;

(** Extraction of the information (variables, multiplexes, etc.) **)
let varlist = fst (List.split vars) ;;
let multlist = fst (List.split mults) ;;
let getbound a = fst (List.assoc a vars) ;;
let getpredec a = snd (List.assoc a vars) ;;
let getformula m = List.assoc m mults ;;

(** Powerset function *)
let powerset (l:resource) =
  let rec ps lp l =
    match l with
      | [] -> [lp]
      | h :: t -> (ps lp t) @ (ps (lp @ [h]) t) in
  ps [] l ;;

(** Extraction of the variable or sign from a discrete path atom **)
let varDpa (t:dpa) = fst t ;;
let signDpa (t:dpa) = snd t ;;


(*********************************************************************)
(*** Extraction of new constraints : Signed of neighbor celerities ***)
(*********************************************************************)

(* The aim of the next functions is to assess the sign of celerity with the constraint :
C_{v,omega,n} * C_{v,omega,(n+1)} => 0,
with "v" a variable,
"omega" a resource,
and "n" an index of the variable in the current state lower than the boundary b_v.
Indeed, for both celerities of a variable v with same resources but an index different,
their sign is identical or one of these celerities is null. *)

(* This function modifies the value of the index with variable "v" and resource "w" set *)
let rec index_sign_celerity (v:var) (w:resource) (n:int) =
  if (n > 1) 
  then (HOp(
          And,
          HAtom(
            GE, 
            HTermRelation(Times, Celerity(v,w,(n-1)), Celerity(v,w,n)), 
            HConstant(0.0)), 
          index_sign_celerity v w (n-1)))
  else
    (HAtom(
      GE,
      HTermRelation(Times, Celerity(v,w,(n-1)), Celerity(v,w,n)),
      HConstant(0.0)));;

(* This function modifies the value of the resources of a variable "v" 
and call the previous function to change the value of the index *)
let rec resource_sign_celerity (v:var) (l) =
  match l with
  | [] -> raise(Invalid_argument "resource_sign_celerity: none resource for the corresponding variable")
  | h::[] -> index_sign_celerity v h (getbound v)
  | h::t -> HOp(And, index_sign_celerity v h (getbound v), resource_sign_celerity v t);;
;;

let rec celerity_reachability_sign v w n m =
  if (n = 0)
  then (
    if (m > 1)
    then (
      HOp(
        And,
        HAtom(LT, Celerity(v,w,m), HConstant(0.0)),
        celerity_reachability_sign v w n (m-1)))
    else (HAtom(LT, Celerity(v,w,m), HConstant(0.0))))
  else (
    if (m > n)
    then (
      HOp(
        And,
        HAtom(LT, Celerity(v,w,m), HConstant(0.0)),
        celerity_reachability_sign v w n (m-1)))
    else (
      if (m = n)
      then (celerity_reachability_sign v w n (m-1))
      else (
        if (m > 0)
        then (
          HOp(
            And,
            HAtom(GT, Celerity(v,w,m), HConstant(0.0)),
            celerity_reachability_sign v w n (m-1)))
        else (HAtom(GT, Celerity(v,w,m), HConstant(0.0))))))
;;

let rec celerity_reachability_index (v:var) (w:resource) (n:int) = 
  let celerity_sign = celerity_reachability_sign v w n (getbound v) in 
  if (n > 0)
  then (
    HOp(
      And, 
      HOp(
        Impl, 
        HAtom(Eq, Celerity(v,w,n), HConstant(0.0)), 
        celerity_sign), 
      celerity_reachability_index v w (n-1)))
  else (
    HOp(
      Impl, 
      HAtom(Eq, Celerity(v,w,n), HConstant(0.0)), 
      celerity_sign))
;; 

let rec celerity_reachability (v:var) (l) =
  match l with
    | [] -> raise(Invalid_argument "celerity_reachability: none resource for the corresponding variable")
    | h :: [] -> celerity_reachability_index v h (getbound v)
    | h :: t -> HOp(And, celerity_reachability_index v h (getbound v), celerity_reachability v t);;
;;

(* This function computes the constraint of the celerities for each variable 
and call the previous functions *)
let rec signed_celerity (l:var list) =
  match l with
  | [] -> raise(Invalid_argument "signed_celerity: none variable")
  | h::[] -> HOp(And, celerity_reachability h (powerset (getpredec h)), resource_sign_celerity h (powerset (getpredec(h))))
  | h::t -> HOp(And,HOp(And, celerity_reachability h (powerset (getpredec h)), resource_sign_celerity h (powerset (getpredec(h)))), signed_celerity t)
;;
