(********************************************)
(*** Simplification of a hybrid condition ***)
(********************************************)

open Types;;
open Standard_functions;;
open Wp_properties;;

(**
This function allows to simplify a (general) hybrid condition.
The simplifications are based on classical Boolean transformations
with some more precise transformations regarding the implication
(covering frequent cases happening in the weakest precondition calculus).
**)

(*
Cette fonction permet la simplification d'une condition hybride générique.
Les simplifications sont basées sur les transformations booléennes classiques
avec quelques transformations plus poussées pour l'implication
(cas courants lors du calcul de la plus faible précondition).
*)

(** Simplify a general hybrid condition **)
let rec simpl_hybridCondition (hc:hybridCondition) =
  let simpl_neg n hc1 = match n with | Neg ->   (* Simplification of Neg *)
    match simpl_hybridCondition hc1 with
      | HBoolean(True) -> HBoolean(False)
      | HBoolean(False) -> HBoolean(True)
      | HNeg(nn, hc2) -> (match nn with | Neg -> hc2)
      | _ -> HNeg(Neg, hc1) in
  let simpl_and hc1 hc2 =   (* Simplification of And *)
    match simpl_hybridCondition hc1 with
      | HBoolean(True) -> simpl_hybridCondition hc2
      | HBoolean(False) -> HBoolean(False)
      | simplhc1 -> match simpl_hybridCondition hc2 with
          | HBoolean(True) -> simplhc1
          | HBoolean(False) -> HBoolean(False)
          | simplhc2 -> HOp(And, simplhc1, simplhc2) in
  let simpl_or hc1 hc2 =   (* Simplification of Or *)
    match simpl_hybridCondition hc1 with
      | HBoolean(True) -> HBoolean(True)
      | HBoolean(False) -> simpl_hybridCondition hc2
      | simplhc1 -> match simpl_hybridCondition hc2 with
          | HBoolean(True) -> HBoolean(True)
          | HBoolean(False) -> simplhc1
          | simplhc2 -> HOp(Or, simplhc1, simplhc2) in
  let simpl_impl hc1 hc2 =   (* Simplification of Impl *)
    let simpl_impl_2 simplc1 simplc2 =
      match simplc2 with
        | HBoolean(True) -> HBoolean(True)
        | HBoolean(False) -> HNeg(Neg, simplc1)
        | simplc2 -> HOp(Impl, simplc1, simplc2) in
    match simpl_hybridCondition hc1 with
      | HBoolean(True) -> simpl_hybridCondition hc2
      | HBoolean(False) -> HBoolean(True)
      | HOp(And, hc_a, hc_b) -> (match simpl_hybridCondition hc2 with
          | HOp(And, hc_c, hc_d) ->
              if hc_a = hc_c || hc_b = hc_c
                then HOp(Impl, HOp(And, hc_a, hc_b), hc_d)
                else if hc_a = hc_d || hc_b = hc_d
                  then HOp(Impl, HOp(And, hc_a, hc_b), hc_c)
                  else simpl_impl_2 (HOp(And, hc_a, hc_b)) (HOp(And, hc_c, hc_d))
          | HOp(Or, hc_c, hc_d) ->
              if hc_a = hc_c || hc_b = hc_c || hc_a = hc_d || hc_b = hc_d
                then HBoolean(True)
                else simpl_impl_2 (HOp(And, hc_a, hc_b)) (HOp(Or, hc_c, hc_d))
          | simplhc2 -> if hc_a = simplhc2 || hc_b = simplhc2
              then HBoolean(True)
              else simpl_impl_2 (HOp(And, hc_a, hc_b)) simplhc2)
      | simplhc1 -> simpl_impl_2 simplhc1 (simpl_hybridCondition hc2) in
  match hc with
    | HNeg(n, hc1) -> simpl_neg n (simpl_hybridCondition hc1)
    | HOp(op, hc1, hc2) -> (match op with
        | And -> simpl_and hc1 hc2
        | Or -> simpl_or hc1 hc2
        | Impl -> simpl_impl hc1 hc2)
    | HAtom(c, HConstant(i1), HConstant(i2)) ->
        HBoolean(boolean_of_bool (eval_comp c i1 i2))
    | _ -> hc ;;

(** Simplify a condition represented as a list of couples **)
let simpl_lhybridCondition (lh:lcondition) =
  List.map (fun (s, hc) -> (s, simpl_hybridCondition hc)) lh ;;