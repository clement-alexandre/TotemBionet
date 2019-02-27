(******************************************)
(*** Automatic replacement of variables ***)
(******************************************)

open Types;;
open Extraction_data;;
open Standard_functions;;
open Set_states;;
open Display;;
open Wp_properties;;
open Simplification_hybrid_condition;;

(**
This part aims at finding automatically a valuation for each variable
(in a general meaning: π_v^i, π'_v^i, C_v,w,n, T) in a given hybrid condition
and re-inject it in the condition in order to simplify it.

The first step consists in extracting all sub-conditions that are
in conjunction in a general hybrid condition.
The extraction would be finer should the condition be in a CNF,
but in practise the obtained conditions are already very close to CNFs.

The second step consists in searching for atoms that are (quasi-)unitary,
that is, that can be reduced to a form: V = const,
where V is a variable (in a general meaning) and const a real constant.
These atoms represent obvious valuations and are removed from the conjunction
to be stored separately.

Finally, these valuations are re-injected and the condition is simplified
accordingly. The whole process can be iterated in order to find new valuations
after this simplification.
**)

(*
Cette partie a pour but de trouver automatiquement une valuation
pour les variables (au sens large : π_v^i, π'_v^i, C_v,w,n, T)
dans une condition hybride et de la réinjecter pour simplification.

La première étape consiste à extraire toutes les sous-conditions qui sont en
conjonction dans une condition hybride générique.
L'extraction serait plus fine sur une condition en CNF,
mais en pratique les conditions sont déjà très proches d'une CNF.

La seconde consiste à chercher les atomes hybrides qui sont (quasi-)unitaires,
c'est-à-dire pouvant se réduire à une forme : V = const,
où V est une variable (au sens large) et const une constante réelle.
Ces atomes représentent des valuations évidentes et sont retirés
de la conjonction et conservés à part pour réinjection.

Enfin, les valuations sont réinjectées et la condition est simplifiée.
L'ensemble des étapes peut être itéré pour trouver de nouvelles valuations
après cette simplification.
*)


(** (Hybrid) valuation : association list of a hybrid term and a real constant **)
(* Note: a discrete valuation in fact correspond to a pseudo-state *)
type valuation = (hybridTerm * float) list ;;

(** Type hybrid conditions conjunction: liste of hybridCondition **)
type hybridConj = hybridCondition list ;;

(** Extract all components of the conjunction **)
let extract_conj (ohc:hybridCondition) =
  let rec rec_extract_conj hc l =
    match hc with
      | HOp(And, hc1, hc2) -> rec_extract_conj hc1 (rec_extract_conj hc2 l)
      | _ -> hc :: l in
  rec_extract_conj ohc [] ;;

(** Score of a hybrid term (count of the number of variables:
    = 0 if contains only constants; > 0 if contains variables) **)
let rec score_hybridTerm = function
  | PiEntrance(_, _) | PiExit(_, _) | Celerity(_, _, _) -> 1
  | HConstant(_) -> 0
  | HTime(t) -> (match t with | VarTime(_) -> 1 | ValueTime(_) -> 0)
  | HTermRelation(_, ht1, ht2) -> (score_hybridTerm ht1) + (score_hybridTerm ht2) ;;

(** Evaluate a hybrid term (requires to contain only constants) **)
let rec eval_hybridTerm (ht:hybridTerm) =
  let exnia = Invalid_argument "eval_hybridTerm: variables cannot be evaluated" in
  match ht with
    | PiEntrance(_, _) | PiExit(_, _) | Celerity(_, _, _) -> raise exnia
    | HConstant(i) -> i
    | HTime(t) -> (match t with | VarTime(_) -> raise exnia | ValueTime(f) -> f)
    | HTermRelation(op, ht1, ht2) ->
        (eval_termSymbol op) (eval_hybridTerm ht1) (eval_hybridTerm ht2) ;;

(** Extract valuations by unitarizing the atoms when possible **)
(* Calls to List.rev allow to maintain the same order between valuations
   and the hybrid atoms from which they are extracted *)
let get_auto_valuations (hconj:hybridConj) (orival:valuation) =
  (* Return the term with a variable on the left and the constant one on the right *)
  let lvar_rconst (ht1:hybridTerm) (ht2:hybridTerm) =
    if score_hybridTerm ht1 = 1 then ht1, ht2 else ht2, ht1 in
      (* Unitarize an atom (simplification as V = const) *)
  let rec reduce_terms (tv:hybridTerm) (tc:hybridTerm) =
    match tv with
      | PiEntrance(_, _) | PiExit(_, _) | Celerity(_, _, _) -> tv, tc
      | HConstant(_) -> raise (Invalid_argument "get_auto_valuations.reduce_terms")
      | HTime(t) -> (match t with
          | VarTime(_) -> tv, tc
          | ValueTime(_) -> raise (Invalid_argument "get_auto_valuations.reduce_terms"))
      | HTermRelation(op, htt1, htt2) ->
          let ttv, ttc = lvar_rconst htt1 htt2 in
            match op with
              | Plus -> reduce_terms ttv (HTermRelation(Minus, tc, ttc))   (* v+x=y or x+v=y *)
              | Minus -> if ttv = htt1
                  then reduce_terms ttv (HTermRelation(Plus, tc, ttc))     (* v−x=y *)
                  else reduce_terms (HTermRelation(Plus, ttv, tc)) ttc     (* x−v=y *)
              | Times -> reduce_terms ttv (HTermRelation(Divide, tc, ttc)) (* v×x=y or x×v=y *)
              | Divide -> if ttv = htt1
                  then reduce_terms ttv (HTermRelation(Times, tc, ttc))    (* v÷x=y *)
                  else reduce_terms (HTermRelation(Times, ttv, tc)) ttc in (* x÷v=y *)
      (* Call to the unitarization function *)
  let call_reduce_terms (ht1:hybridTerm) (ht2:hybridTerm) =
    let tv, tc = lvar_rconst ht1 ht2 in reduce_terms tv tc in
      (* Iteration on the conjunction represented as a list *)
  let rec rec_valuations (hconjpre:hybridConj) (hconjpost:hybridConj) (lval:valuation) =
    match hconjpre with
      | [] -> List.rev hconjpost, List.rev lval
      | h :: t -> match h with
          | HAtom(Eq, ht1, ht2) -> if (score_hybridTerm ht1) + (score_hybridTerm ht2) = 1
                    (* If Eq and score = 1, Then extract a valuation *)
              then let v, const = call_reduce_terms ht1 ht2 in
                rec_valuations t hconjpost ((v, eval_hybridTerm const) :: lval)
                    (* Else make no modification and continue with the next element *)
              else rec_valuations t (h :: hconjpost) lval
          | _ -> rec_valuations t (h :: hconjpost) lval in
  rec_valuations hconj [] (List.rev orival) ;;

(** Apply a discrete valuation (equivalent to a state) to a discrete term **)
let rec valuation_discreteTerm (s:state) (dt:discreteTerm) =
  match dt with
    | DConstant(_) -> dt
    | Eta(v) -> DConstant(value_eta_var s v)
    | DTermRelation(op, dt1, dt2) ->
        DTermRelation(op, valuation_discreteTerm s dt1, valuation_discreteTerm s dt2) ;;

(** Apply a hybrid valuation (association list) to a hybrid term **)
let rec valuation_hybridTerm (l:valuation) (ht:hybridTerm) =
  let maybe_replace =
    if List.mem_assoc ht l
      then HConstant(List.assoc ht l)
      else ht in
  match ht with
    | HConstant(_) -> ht
    | PiEntrance(_, _) | PiExit(_, _) | Celerity(_, _, _) -> maybe_replace
    | HTime(t) -> (match t with
          | VarTime(_) -> maybe_replace
          | ValueTime(_) -> ht)
    | HTermRelation(s, ht1, ht2) ->
        HTermRelation(s, valuation_hybridTerm l ht1, valuation_hybridTerm l ht2) ;;

(** Apply a valuation to a hybrid condition **)
let valuation_hybridCondition (s:state) (l:valuation) (ohc:hybridCondition) =
  let rec rec_valuation hc =
    match hc with
      | HBoolean(_) -> hc
      | HNeg(n, hc1) -> (match n with | Neg -> HNeg(Neg, rec_valuation hc1))
      | HOp(op, hc1, hc2) -> HOp(op, rec_valuation hc1, rec_valuation hc2)
      | HAtom(comp, ht1, ht2) ->
          HAtom(comp, valuation_hybridTerm l ht1, valuation_hybridTerm l ht2)
      | HDAtom(comp, dt1, dt2) ->
          HDAtom(comp, valuation_discreteTerm s dt1, valuation_discreteTerm s dt2) in
  simpl_hybridCondition (rec_valuation ohc) ;;

(**
The following functions allow to apply the above functions
to lcondition's, that is, to lists of (state, hybrid condition)
generated by the hybrid weakest precondition calculus.
**)

(*
Les fonctions suivantes permettent d'appliquer les fonctions ci-dessus
aux lcondition, c'est-à-dire aux listes de (état, condition hybride)
générés par le calcul de plus faible précondition.
*)

(** Type lconjunction: liste of (state, hybridCondition list)
    representing all the conjunctions of a hybrid condition for each state **)
type lconjunction = (state * hybridConj) list ;;

(** Super-valuation: discrete state & valuation on hybrid terms
    attached to a given hybrid condition **)
type super_valuation = state * valuation * hybridConj ;;

(** Extract the conjunctions of a lcondition (iteration on its elements) **)
let lextract_conj (ohc:lcondition) =
  List.fold_left
    (fun l (s, hc) -> (s, (extract_conj (simpl_hybridCondition hc))) :: l)
    [] ohc ;;

(** Add an empty valuation to create a basic super_valuation **)
let add_emptyvaluation (ohc:lconjunction) =
  List.map (fun (s, hconj) -> (s, [], hconj)) ohc ;;

(** Extract the valuations of a quasi-lcondition (state, hybridConj) **)
let lget_auto_valuations (lval:super_valuation list) =
  List.rev
    (List.fold_left
      (fun l (s, valu, hcl) -> let resconj, resval = get_auto_valuations hcl valu in
        (s, resval, resconj) :: l)
      [] lval) ;;


(* Create a list with all pi_entrance and pi_exit entities and their interval *)
let pi_string_list (p:pathLanguage) = 
  let rec number_of_tuple (p:pathLanguage) (n:int)  =
    match p with
      | Nothing -> 0
      | Tuple(t, a , dpa) -> n
      | Seq(p1, p2) -> number_of_tuple p2 (n+1)
    in  
  let rec add_pi_element v n l = 
    if (n >= 0)
    then (add_pi_element v (n-1) (["pi_entrance_" ^ v ^ "_" ^ (string_of_int n)] @ (["pi_exit_" ^ v ^ "_" ^ (string_of_int n)] @ l)) )
    else l
    in 
  let rec find_all_var v l n =
    match v with
      | [] -> raise(Invalid_argument "find_all_var: varlist empty")
      | h :: [] -> (add_pi_element h n []) @ l
      | h :: t -> find_all_var t ((add_pi_element h n []) @ l) n
  in find_all_var varlist [] (number_of_tuple p 1) ;;


let cyclic_behaviour (p:pathLanguage) = 
  let rec number_of_tuple (p:pathLanguage) (n:int)  =
    match p with
      | Nothing -> 0
      | Tuple(t, a , dpa) -> n
      | Seq(p1, p2) -> number_of_tuple p2 (n+1)
    in
  let rec cyclic_constraints v l n =
    match v with
      | [] -> raise(Invalid_argument "cyclic_constraints: varlist empty")
      | h :: [] -> [HAtom(Eq, PiEntrance(h,0), PiEntrance(h,n))] @ l
      (* | h :: [] -> [HAtom(Eq, PiEntrance(h,0), PiEntrance(h,n))] @ ([HAtom(Eq, PiExit(h,0), PiExit(h,n))] @ l) *)
      | h :: t -> cyclic_constraints t ([HAtom(Eq, PiEntrance(h,0), PiEntrance(h,n))] @ l) n
      (* | h :: t -> cyclic_constraints t ([HAtom(Eq, PiEntrance(h,0), PiEntrance(h,n))] @ ([HAtom(Eq, PiExit(h,0), PiExit(h,n))] @ l)) n *)
  in cyclic_constraints varlist [] (number_of_tuple p 1);;

(* Create a list with all celerity entities and their interval *)
let cel_string_list (varl) =
  let rec change_index v w n l =
    if (n > 0)
    then (change_index v w (n-1) (["C_" ^ v ^ "__" ^ (string_of_list_delim string_of_mult "_" "" "" w) ^ "__" ^ (string_of_int n)] @ l))
    else (["C_" ^ v ^ "__" ^ (string_of_list_delim string_of_mult "_" "" "" w) ^ "__" ^ (string_of_int n)] @ l)
  in
  let rec add_cel_element v wl n l =
    match wl with
      | [] -> raise(Invalid_argument "add_cel_element: predecessor of the current node empty")
      | h :: [] -> change_index v h n l
      | h :: t -> add_cel_element v t n (change_index v h n l)
    in
  let rec find_all_var v l =
    match v with
      | [] -> raise(Invalid_argument "find_all_var: varlist empty")
      | h :: [] -> add_cel_element h (powerset(getpredec(h))) (getbound(h)) l
      | h :: t -> find_all_var t (add_cel_element h (powerset(getpredec(h))) (getbound(h)) l)
    in find_all_var varl [];;

let cel_list = cel_string_list varlist;;