(*********************)
(*** Main function ***)
(*********************)

(* This is the main function of the computational tool
Starting from the data of the parser, we identify the
properties to define, and reduce the constraints obtained.
Finally, we write the result inside an output file which
will be used by the solver called AbSolute. *)

open Types;;
open Extraction_data;;
open Standard_functions;;
open Valuation;;
open Wp_computation;;
open Debugging;;
open Dnf_constraints;;
open Output_files;;
open Printf;;
open Set_states;;
open Strongest_postcondition;;


let simplification x =
  simplify_cstr_cel (simplify_cstr_pi x);;

(** Generic definition of a fixpoint **)
let rec fixpoint x0 func =
  let x = func x0 in
    (* print_endline ("lval = \n" ^ print_list_lval x); *)
    (* print_endline ("hConj = \n" ^ print_hConj x); *)
    (* print_endline ("hDisj = \n" ^ print_hDisj x); *)
    (* print_endline ("lval = \n" ^ print_list_lval x0); *)
    (* print_endline ("hConj = \n" ^ print_hConj x0); *)
    (* print_endline ("hDisj = \n" ^ print_hDisj x0); *)
    if x = x0
      then x0
      else fixpoint x func ;;



(* TEMPORARY These two functions display the discrete value of each variable in the precondition.
WARNING: several states can exist in the precondition (see print_lpost) *)
(* For the moment, we focus on the first state *)
let rec print_intl l =
  match l with
    | [] -> ""
    | h::[] -> string_of_int(h)
    | h::t -> string_of_int(h) ^ " " ^ print_intl t
;;

let rec print_lpost l =
  match l with
    | [] -> ""
    | h::[] -> print_intl (h)
    | h::t -> print_intl (h) ^ "; Other available state: " ^ print_lpost t
;;


let _ =
  (* signed_celerity is a function adding the constraint : 
  C_{v,omega,n} * C_{v,omega,(n+1)} => 0 
  and C_{v,omega,n} = 0 => 
    ∀ k in [0;n-1],C_{v,omega,n} > 0  ∧ ∀ k in [n+1;b_v],C_{v,omega,n} < 0*)
  let h0 = HOp(And, signed_celerity varlist, first_h0) in
  let post0 = (d0, h0) in 
  (** Final result: call the functions above on the example **)
  let wp = computeWP prog0 post0 in
  let simpl_wp = (simpl_valuation << filter_out_true) wp in
  let list_of_cel = celerities_of_wp prog0 post0 in
  (* print_endline (print_cel list_of_cel); *)
  (* create_dinit varlist (discreteWPSP prog0 (discretePart post0)); *)
  (* print_endline(printl varlist);
  print_endline (print_lpost (discreteWPSP prog0 (discretePart post0))); *)
  (* print_lsuper_valuation simpl_wp ; *)
  let l = separate_disj simpl_wp list_of_cel in
  let c = cyclic_constraint isCyclic l (cyclic_behaviour prog0) in
  let dinitlist = get_ze_states (discreteWP prog0 (discretePart post0)) in
  create_dinit (Sys.argv.(3)) varlist (List.nth dinitlist 0);
  (* print_endline(print_lpost dinitlist); *)
  (* print_endline ("lval = \n" ^ print_list_lval c); *)
  (* print_endline ("hConj = \n" ^ print_hConj c); *)
  (* print_endline ("hDisj = \n" ^ print_hDisj c); *)
  (* let dnf_result = simplify_cstr_pi (c) in
  print_endline ("hConj = " ^ print_hConjl (dnf_result));
  print_endline ("hDisj = " ^ print_hDisjl (dnf_result)); *)
  (* let dnf_first_simplification = simplify_cstr_cel(simplify_cstr_pi (c)) in *)
  (* print_endline ("hConj = " ^ print_hConjl (dnf_first_simplification)); *)
  (* print_endline ("hDisj = " ^ print_hDisjl (dnf_first_simplification)); *)
  let simpl = fixpoint c simplification in
  (* print_endline ("lval = \n" ^ print_list_lval simpl);   *)
  (* print_endline ("hDisj = \n" ^ print_hDisjl simpl); *)
  (* print_endline ("hConj = \n" ^ print_hConj simpl); *)
  let dnf_result = simplify_cstr_touch_delay simpl in
  (* print_endline ("lval = \n" ^ print_list_lval dnf_result);   *)
  (* print_endline ("hDisj = \n" ^ print_hDisjl_with_simplify_cstr_touch_delay (dnf_result)); *)
  let final_hconj = simplify_hconj (replace_HOp_in_hconl (get_hconj dnf_result)) in
  (* print_endline ("hConj = \n" ^ print_hConjl_simplified (final_hconj)); *)
  let lval = get_lval dnf_result in
  let pos_list = pi_string_list prog0 in
  create_file Sys.argv.(2) lval (final_hconj) (select_hdisj dnf_result) pos_list cel_list
;;