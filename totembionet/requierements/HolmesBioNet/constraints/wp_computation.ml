(***********************************************)
(*** Computation of the weakest precondition ***)
(***********************************************)

open Types;;
open Standard_functions;;
open Dnf_discrete_condition;;
open Strongest_postcondition;;
open Wp_properties;;
open Valuation;;
open Extraction_data;;

(** Compute the weakest precondition **)
(**
Result: lcondition = (state * hybridCondition) list
Each element of the list represents a compatible discrete state (first element)
to which corresponds a hybrid weakest precondition (second element).
**)
(*
Résultat : lcondition = (state * hybridCondition) list
Chaque élément de la liste représente un état compatible (premier élément)
auquel correspond une plus faible condition hybride (deuxième élément).
*)
let computeWP (prog:pathLanguage) (post:property) =
  hybridWP prog (hybridPart post) (discreteWPSP prog (discretePart post)) ;;

(** Simplify by searching for valuations **)
(**
Result: super_valuation list = (state * valuation * hybridConj) list
Each element represents a compatible discrete state (first element),
all found valuations (second element)
and the non-reducible part of the hybrid condition (third element).
A valuation is a couple (ht, const) of a hybridTerm and a float
corresponding to an atom of the form ht = const.
The non-reducible part of the hybrid condition is represented as a list
of hybridCondition's.
The lists of valuations and hybridConj's are virtually in conjunction
between them and so are all elements inside these lists.
**)
(*
Résultat : super_valuation list = (state * valuation * hybridConj) list
Chaque élément représente un état compatible (premier élément),
toutes les valuations trouvées (deuxième élément)
et la partie non réductible de la condition hybride (troisième élément).
Une valuation est un couple (ht, const) d'un hybridTerm et d'un float
correspondant à un atome ht = const.
La partie non réductible de la condition hybride est représentée sous la forme
d'une liste de hybridCondition.
Les listes de valuations et de hybridConj sont virtuellement en conjonction
entre elles, et les éléments au sein de chaque liste le sont aussi.
*)

let simpl_valuation (prop:lcondition) =
  fixpoint
    (add_emptyvaluation (lextract_conj prop))
    (lget_auto_valuations) ;;


(** Filters out useless "True" atoms in the final hybridConj's **)
let filter_out_true (lsval:super_valuation list) =
  List.map
    (fun (s, v, hconj) ->
      (s, v, List.filter (((=) (HBoolean(True))) << not) hconj))
    lsval ;;


(* The function below identifies the celerities observed in the Hoare triple. *)
let celerities_of_wp (prog:pathLanguage) (post:property) =
  let rec in_list elt l =
    match l with
      | [] -> false
      | h :: t -> if (elt = h) then (true) else (in_list elt t)
    in
  let rec assess_cel state varlist omega lf =
    match (state, varlist, omega) with
      | (hs :: ts, hv :: tv, hom :: tom) ->
          if (in_list (Celerity(hv, hom, hs)) lf)
          then (assess_cel ts tv tom lf)
          else ([Celerity(hv, hom, hs)] @ (assess_cel ts tv tom lf))
      | (_, _, _) -> []
    in
  let invert_symbol s =
    match s with
      | PlusSymbol -> MinusSymbol
      | MinusSymbol -> PlusSymbol
    in
  let rec assess_prog prog s lf =
    match prog with
      | Nothing -> ([],[])
      | Tuple(time, a, dpa) ->
        let prev_state = neighbour_state s (varDpa dpa) (invert_symbol (signDpa dpa)) in
        let omega = eval_resource_omega_varlist prev_state in
          (prev_state, (assess_cel prev_state varlist omega lf) @ lf)
      | Seq(p1,p2) -> 
        let (current_state, celList) = assess_prog p2 s lf in
          assess_prog p1 current_state (celList @ lf) 
    in
  let rec search_all_lstate prog lstate lf =
    match lstate with
      | [] -> lf
      | h :: t -> search_all_lstate prog t ((snd (assess_prog prog h lf)) @ lf)
    in search_all_lstate prog (discreteWPSP prog (discretePart post)) [];;

