(********************************)
(*** Creation of output files ***)
(********************************)

open Types;;
open Display;;
open Printf;;

(* get_lval function select lvaluation list to display into the output files *)
let get_lval sval =
  let rec select_lval s =
    match s with
      | (lval, (_,_)) -> lval
    in
  let rec select_element s l =
    match s with
      | [] -> raise(Invalid_argument "select_element: lvaluation empty")
      | h::[] -> [select_lval h] @ l
      | h::t -> select_element t ([select_lval h] @ l)
    in
    select_element sval [];;

(* This function creates a file containing the level of each variable in the precondition
This file will be used for the initialization of the simulation *)
let create_dinit file varl dinitl =
  let rec write_line vl dl n =
    if (n < (List.length vl) -1) && (n < (List.length dl) -1)
    then
      "Eta(" ^ List.nth vl n ^ ") = " ^ string_of_int(List.nth dl n) ^ "\n" ^ write_line vl dl (n+1)
    else
      "Eta(" ^ List.nth vl n ^ ") = " ^ string_of_int(List.nth dl n)
    in
  let write f vl dl =
    let oc = open_out (f) in 
    fprintf oc "%s" (write_line vl dl 0);
    close_out oc
  in write file varl dinitl
;;


(* This function creates the output file. Each line of the constraints in
the file is separated by a conjunction connective. *)

let create_file file lval_list hconj_list hdisj_list pl cl =
  let rec write_cst oc v =
    match v with
      | [] -> raise(Invalid_argument "write_cst: valuation list empty")
      | h :: [] ->
        (match h with
          | (ht, fl) -> (
            match ht with
            | PiEntrance(v,i) | PiExit(v,i) -> 
              fprintf oc "\t%s;\n" (string_of_couple string_of_hybridTerm_solver string_of_float "=" "" "" (ht,fl))
            | Celerity(v,r,i) ->
              fprintf oc "\t%s;\n" (string_of_couple string_of_hybridTerm_solver string_of_float "=" "" "" (ht,fl))
            | _ -> raise(Invalid_argument "write_cst: The variables are only pi_entrance, pi_exit and celerities")))
      | h :: t ->
        (match h with
          | (ht, fl) -> 
            (match ht with
              | PiEntrance(v,i) | PiExit(v,i)  -> fprintf oc "\t%s;\n" (string_of_couple string_of_hybridTerm_solver string_of_float "=" "" "" (ht,fl))
              | Celerity(v,r,i) -> fprintf oc "\t%s;\n" (string_of_couple string_of_hybridTerm_solver string_of_float "=" "" "" (ht,fl))
              | _ -> raise(Invalid_argument "write_cst: The variables are only pi_entrance, pi_exit and celerities"))); write_cst oc t
    in
  let rec write_hconj oc hc =
    match hc with
      | [] -> raise(Invalid_argument "write_hconj: hybridConjunction list empty")
      | h :: [] -> fprintf oc "\t%s;\n" (string_of_hybridCondition_solver h)
      | h :: t -> fprintf oc "\t%s;\n" (string_of_hybridCondition_solver h); write_hconj oc t
    in
  let rec is_in_constant elt l =
    match l with
      | [] -> false
      | h :: [] ->
        (match h with
          | (elt1, fl1) -> if (elt = (string_of_hybridTerm_solver elt1)) then (true) else (false))
      | h :: t ->
        (match h with
          | (elt1, fl1) -> if (elt = (string_of_hybridTerm_solver elt1)) then (true) else (is_in_constant elt t))
    in
  let rec write_list oc l v s =
    match l with
      | [] -> raise(Invalid_argument "write_pi: pi_entrance and pi_exit list empty")
      | h :: [] -> if (not (is_in_constant h v)) then (fprintf oc "\treal %s" (h ^ s))
      | h :: t -> if (not (is_in_constant h v)) then (fprintf oc "\treal %s" (h ^ s); write_list oc t v s) else (write_list oc t v s)
    in
  (* let rec write_list_temp oc l v s =
    match l with
      | [] -> raise(Invalid_argument "write_pi: pi_entrance and pi_exit list empty")
      | h :: [] -> fprintf oc "\treal %s" (h ^ s)
      | h :: t -> fprintf oc "\treal %s" (h ^ s); write_list_temp oc t v s
    in *)
  let list_is_empty l =
    match l with
      | [] -> true
      | h :: t -> false
    in
  let cr pl cl file h v hc n  =
    let oc = open_out (file ^ "_" ^ (string_of_int n) ^ ".abs") in
      fprintf oc "constants{\n";
      write_cst oc v;
      fprintf oc "}\n\ninit{\n";
      (* write_list_temp oc pl v (" = [0;1];\n"); *)
      write_list oc pl v (" = [0;1];\n");
      write_list oc cl v (" = [-50;50];\n");
      fprintf oc "}\n\nconstraints{\n";
      (* write_cst oc v; *)
      write_hconj oc hc;
      if (list_is_empty h)
      then (fprintf oc "}";)
      else fprintf oc "%s;\n}" (string_of_tuple_list string_of_hybridCondition_solver ";\n" h " || ");
      close_out oc
    in
    (* Look for each elt in hdisj list and call create function to create file on each
    lval and hconj elt *)
  let rec select_each_elt f pl cl hd v hc n  =
    match hd with
      | [] -> cr pl cl f [] v hc n 
      | h::[] -> cr pl cl f h v hc n 
      | h::t -> cr pl cl f h v hc n ; select_each_elt f pl cl t v hc n 
    in
    (* Return a tuple (first elt of hconj list, hconj list without first elt) *)
  let select_elt_hconj_list hconj =
    match hconj with
      | [] -> raise(Invalid_argument "select_elt_hconj_list: Error")
      | h::[] -> (h,[])
      | h::t -> (h,t)
    in
    (* Look for each elt in lval and in hconj lists *)
  let rec select_element file lvall hconjl hdisjl pl cl n  =
    match lvall with
      | [] -> raise(Invalid_argument "select_element: Error")
      | h1::[] -> select_each_elt file pl cl hdisjl h1 (fst(select_elt_hconj_list hconjl)) n 
      | h1::t1 -> select_each_elt file pl cl hdisjl h1 (fst(select_elt_hconj_list hconjl)) n ; select_element file t1 (snd(select_elt_hconj_list hconjl)) hdisjl pl cl (n+1)
    in select_element file lval_list hconj_list hdisj_list pl cl 1
;;