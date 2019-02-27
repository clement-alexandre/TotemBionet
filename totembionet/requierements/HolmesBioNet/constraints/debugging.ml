(************************************************)
(*** Display for debug the computational tool ***)
(************************************************)

open Display;;
open Valuation;;


let print_hConj h =
  let rec print_all_hConj l =
    match l with
      | [] -> ""
      | h::[] -> string_of_hybridConj (fst (snd h))
      | h::t -> string_of_hybridConj (fst (snd h)) ^ "\n" ^ print_all_hConj t
    in print_all_hConj h
;;

let print_hDisj h =
  let rec print_all_hConj l =
    match l with
      | [] -> ""
      | h::[] -> string_of_hybridConj (snd (snd h))
      | h::t -> string_of_hybridConj (snd (snd h)) ^ "\n" ^ print_all_hConj t
    in print_all_hConj h
;;

let print_hDisjl sval =
  let get_tuple s =
    match s with
      | (lval, cstr) -> string_of_hybridConj (snd cstr)
    in
  let rec eval_all_element s =
    match s with
      | [] -> raise(Invalid_argument "eval_all_element: list sval empty")
      | h :: [] -> get_tuple h
      | h :: t -> (get_tuple h) ^ "\n\n" ^ (eval_all_element t)
    in eval_all_element sval;;


let print_hDisjl_with_simplify_cstr_touch_delay sval =
	let rec get_each_elt l =
		match l with
			| [] -> ""
			| h::[] -> string_of_hybridCondition (fst h) ^ " ∨ " ^ string_of_hybridCondition (snd h) 
			| h::t -> string_of_hybridCondition (fst h) ^ " ∨ " ^ string_of_hybridCondition (snd h) ^ "\n" ^ get_each_elt t
	in
	let get_tuple s =
		match s with
			| (lval, cstr) -> get_each_elt (snd cstr)
	in
	let rec eval_all_element s =
		match s with
			| [] -> raise(Invalid_argument "eval_all_element: list sval empty")
			| h :: [] -> get_tuple h
			| h :: t -> (get_tuple h) ^ "\n\n" ^ (eval_all_element t)
	in eval_all_element sval;;

let print_hConjl sval =
  let get_tuple s =
    match s with
      | (lval, cstr) -> string_of_hybridConj (fst cstr)
    in
  let rec eval_all_element s =
    match s with
      | [] -> raise(Invalid_argument "eval_all_element: list sval empty")
      | h :: [] -> get_tuple h
      | h :: t -> (get_tuple h) ^ "\n\n" ^ (eval_all_element t)
    in eval_all_element sval;;

let print_list_lval v =
  let rec print_all_lval l =
    match l with 
      | [] -> ""
      | h::[] -> string_of_lval (fst h)
      | h::t -> (string_of_lval (fst h)) ^ "\n\n" ^ (print_all_lval t)
    in print_all_lval v
;;

let print_hConjl_simplified hconj =
  let rec print_hconj_list l =
    match l with
      | [] -> ""
      | h :: [] -> string_of_hybridCondition h ^ "\nEnd of hconj list"
      | h :: t -> string_of_hybridCondition h ^ "\n" ^ print_hconj_list t
    in
  let rec eval_each_element l =
    match l with
      | [] -> ""
      | h :: [] -> print_hconj_list h
      | h :: t -> print_hconj_list h ^ "\nNo other hconj list\n" ^ (eval_each_element t)
    in eval_each_element hconj;;