(************************************)
(*** Identify Disjunctive formula ***)
(************************************)

open Display;;
open Types;;
open Valuation;;
open Extraction_data;;
open Debugging;;


(* Separate implication expressions of each hybridConj of the super_valuation type 
A list is created with a tuple of the form 
(valuation , (hybridConj without "disjunctive formula" , hybridConj with "disjunctive formula"))
The hybrid condition HNeg and Hop(Impl, _, _) are transformed to disappear *)
let separate_disj sval cl =
  let neg_comp = function
    (* Negation of a comparator *)
    | Eq -> NEq
    | NEq -> Eq
    | LE -> GT
    | LT -> GE
    | GE -> LT
    | GT -> LE 
    in
  let neg_operator = function
    | And -> Or
    | Or -> And
    | Impl -> raise(Invalid_argument "neg_operator: should analyse And/Or operators only")
    in
  let rec reverse_comp h =
    match h with
      | HAtom(comp, h1, h2) -> HAtom((neg_comp comp), h1, h2)
      | HDAtom(comp, h1, h2) -> HDAtom((neg_comp comp), h1, h2)
      | HOp(op, HAtom(comp1, PiExit(v1,i1),float1), HAtom(comp2, PiExit(v2,i2),float2)) -> HOp((neg_operator op), HAtom(Eq, PiExit(v1,i1), HConstant(0.0)), HAtom(Eq, PiExit(v2,i2), HConstant(1.0)))
      (* The case HOp only includes the continuous disjunctions and so acts on the fractional parts. 
      We give a specific result of this case since π_v ≤ 0 means π_v = 0 *)
      | HOp(op, h1, h2) -> HOp((neg_operator op), (reverse_comp h1), (reverse_comp h2))
      | other -> other
    in
  let rec replace_neg h = (* Replace the neg constructor *)
    match h with
      | HNeg(neg, h1) -> reverse_comp h1
      | HOp(Impl, h1, h2) -> HOp(Or, (reverse_comp h1), h2)
      | HOp(And, h1, h2) -> HOp(And, h1, h2)
      | HOp(Or, h1, h2) -> HOp(Or, h1, h2)
      | other -> raise(Invalid_argument("replace_neg: error to select the disjunctions:\n" ^ (string_of_hybridCondition other)))
    in
  let rec find_disj h b = (* Find the Impl operator and the And operator included in neg constructor  *)
    match h with 
      | HOp(op, hc1, hc2) -> if ((op = Impl && b) || (op = And && b = false)) then (true) else (false)
        (* Above we only keep ¬(A∧B) and (A⇒B). ¬(A⇒B) can be replaced by (A∧¬B) *)
      | HNeg(neg, h1) -> find_disj h1 false 
      | otherConstructor -> false
    in
  let rec cel_in_list ht cl =
    match cl with
      | [] -> false
      | h :: t -> if (ht = h) then (true) else (cel_in_list ht t)
    in
  let rec hterm_in_list ht cl =
    match ht with
      | PiEntrance(_,_) | PiExit(_,_) | HConstant(_) | HTime(_) -> true
      | Celerity(v,w,n) -> cel_in_list (Celerity(v,w,n)) cl
      | HTermRelation(s, ht1, ht2) -> hterm_in_list ht1 cl && hterm_in_list ht2 cl
    in
  let rec hc_defined_in_list hc cl =
    match hc with
      | HBoolean(b) -> true
      | HNeg(n, hc1) -> hc_defined_in_list hc1 cl
      | HAtom(c, ht1, ht2) -> hterm_in_list ht1 cl && hterm_in_list ht2 cl
      | HDAtom(c, dt1, dt2) -> true
      | HOp(op, hc1, hc2) -> hc_defined_in_list hc1 cl && hc_defined_in_list hc2 cl
    in
  let rec search_impl lc ld hconj cl = (* Create a tuple (list of hConj without disjunction , list of tuple with each expression without neg operator) = (hConj list, [(h1,h2);...]) *)
    match hconj with
      | [] -> raise(Invalid_argument "search_impl: none hybridConjunction")
      (* Example of Slide(u) without knowing Slide + or Slide - *)
      | (HOp(Or, HOp(And, ht1, HOp(Impl, ht2, ht3)), HOp(And, ht4, HOp(Impl, ht5, ht6))))::[] ->
        (lc, [HOp(Or, HOp(And, ht1, (replace_neg (HOp(Impl, ht2, ht3)))), HOp(And, ht4, (replace_neg (HOp(Impl, ht5, ht6)))))] @ ld)
      | h :: [] -> 
        if (find_disj h true) 
        then (
          if (hc_defined_in_list h cl)
          then (lc, ([replace_neg h] @ ld))
          else (lc, ld)) 
        else (([h] @ lc), ld)
      (* Example of Slide(u) without knowing Slide + or Slide - *)
      | (HOp(Or, HOp(And, ht1, HOp(Impl, ht2, ht3)), HOp(And, ht4, HOp(Impl, ht5, ht6))))::t ->
        let new_ld = ([HOp(Or, HOp(And, ht1, (replace_neg (HOp(Impl, ht2, ht3)))), HOp(And, ht4, (replace_neg (HOp(Impl, ht5, ht6)))))] @ ld) in
        search_impl lc new_ld t cl
      | h :: t -> 
        if (find_disj h true) 
        then (
          if (hc_defined_in_list h cl)
          then (search_impl lc ([replace_neg h] @ ld) t cl)
          else (search_impl lc ld t cl))
        else (search_impl ([h] @ lc) ld t cl)
    in
  let create_tuple s cl = (* Create a tuple (valuation, (list of hConj without disjunction, list of disjunction) ) *)
    match s with
      | (state, lval, hconj) -> (lval, (search_impl [] [] hconj cl))
    in
  let rec select_each_sval s cl lr = (* Select each element of the list *)
    match s with
      | [] -> raise(Invalid_argument "select_each_sval: not a super_valuation type or empty")
      | h :: [] -> [create_tuple h cl] @ lr
      | h :: t -> select_each_sval t cl ([create_tuple h cl] @ lr)
    in select_each_sval sval cl [];;


(********************************************************)
(*** Identify constraints coming from cyclic behavior ***)
(********************************************************)

(* This function links positions between the first 
and the last elementary paths which are the same
when it is declared in the input file. *)
let add_cyclic_pi sval lpi =
  let rec eval_each_element l lpi =
    match l with
      | [] -> raise(Invalid_argument "add_cyclic_pi funciton: list svaluation empty")
      | (lval, cstr)::[] -> [(lval, ((fst cstr) @ (lpi), snd cstr))]
      | (lval, cstr)::t -> [(lval, ((fst cstr) @ (lpi), snd cstr))] @ (eval_each_element t lpi)
    in eval_each_element sval lpi
;;

let cyclic_constraint boolean s lpi =
  if boolean
  then (add_cyclic_pi s lpi)
  else s
;;


(********************)
(*** Simplify DNF ***)
(********************)

(* This part simplifies constraints of positions, celerities and 
touch delays when there exist. A fixpoint computation is applied
using these functions until finding the minimal simplification. *)


(* The function below determines if some constraints of position (included in 
"disjunctive formula") exist. We search the current constraint in the 
hybridConjunction and lvaluation lists. If it doesn't exist, we let the result 
in the hybridDisjonction list and assess the constraints of the celerities. *)
let simplify_cstr_pi sval =
  let rec in_lval pi fl lval =
    match lval with
      | [] -> raise (Invalid_argument "in_lval : lval should not be empty")
      | h :: [] -> (match h with
              | (ht1,f) -> (ht1 = pi) && (f = fl))
      | h :: t -> (match h with
              | (ht1,f) -> ((ht1 = pi) && (f = fl)) || (in_lval pi fl t))      
    in
  let rec in_lval_neq pi fl lval =
    match lval with
      | [] -> raise (Invalid_argument "in_lval : lval should not be empty")
      | h :: [] -> (match h with
              | (ht1,f) -> if (pi = ht1) 
              then (
                if ((fl = 0.0 && f = 1.0)) || ((fl = 1.0 && f = 0.0))
                then ("exist")
                else ("opposite")
              )
              else "notexist")
      | h :: t -> (match h with
              | (ht1,f) -> if (pi = ht1) 
                then (
                  if ((fl = 0.0 && f = 1.0)) || ((fl = 1.0 && f = 0.0))
                  then ("exist")
                  else ("opposite")
                )
                else (in_lval_neq pi fl t))
    in
  let identify_comp c =
    if (c = Eq)
    then ("exist")
    else (
      if (c = NEq || c = LT  || c = GT)
      then ("opposite")
      else ("notexist"))
    in
  let identify_opp_comp c =
    if (c = Eq)
    then ("opposite")
    else ("notexist")
    in
  let identify_comp_and_float fl1 fl2 comp =
    if (fl1 = fl2)
    then (identify_comp comp)
    else (identify_opp_comp comp)
    in
  let rec cstr_in_hcl_with_eq hcl pi fl1 =
    match hcl with
      | [] -> raise(Invalid_argument "cstr_in_hcl_with_eq : Error, hybridConjunction list empty")
      | (HAtom(comp, PiExit(v,i), HConstant(fl2)))::[] -> 
        if (pi = PiExit(v,i)) 
        then (identify_comp_and_float fl1 fl2 comp) 
        else ("notexist")
      | h::[] -> "notexist"
      | (HAtom(comp, PiExit(v,i), HConstant(fl2)))::t -> 
        if (pi = PiExit(v,i)) 
        then (
          let v = (identify_comp_and_float fl1 fl2 comp) in
          if (v = "notexist")
          then (cstr_in_hcl_with_eq t pi fl1)
          else (v)) 
        else (cstr_in_hcl_with_eq t pi fl1)
      | h::t -> cstr_in_hcl_with_eq t pi fl1
    in
  let identify_comp_min c =
    if (c = NEq || c = GT)
    then ("exist")
    else ("notexist")
    in
  let identify_comp_max c =
    if (c = NEq || c = LT)
    then ("exist")
    else ("notexist")
    in
  let identify_comp_and_float_with_neq fl1 fl2 comp =
    if (fl1 = fl2 && fl1 = 0.)
    then (identify_comp_min comp)
    else (
      if (fl1 = fl2 && fl1 = 1.)
      then (identify_comp_max comp)
      else ("notexist"))
    in
  let rec cstr_in_hcl_with_neq hcl pi fl1 =
    match hcl with
      | [] -> raise(Invalid_argument "cstr_in_hcl_with_neq : Error, hybridConjunction list empty")
      | (HAtom(comp, PiExit(v,i), HConstant(fl2)))::[] -> 
        if (pi = PiExit(v,i)) 
        then (identify_comp_and_float_with_neq fl1 fl2 comp) 
        else ("notexist")
      | h::[] -> "notexist"
      | (HAtom(comp, PiExit(v,i), HConstant(fl2)))::t -> 
        if (pi = PiExit(v,i))
        then (
          let v = (identify_comp_and_float_with_neq fl1 fl2 comp) in
          if (v = "notexist")
          then (cstr_in_hcl_with_neq t pi fl1)
          else (v))
        else (cstr_in_hcl_with_neq t pi fl1)
      | h::t -> cstr_in_hcl_with_neq t pi fl1
    in
  let in_lc ha2 lc =
    match ha2 with
      | HOp(And, ht1, ht2) ->  [ht1] @ ([ht2] @ lc)
      | other -> [other] @ lc
    in
  let contain_logical_connective h =
    match h with
      | HOp(Or, ht1, ht2) -> "Or"
      | HOp(And, ht1, ht2) -> "And"
      | other -> "no_logical_connective"
    in
  let find_cstr hcl h lc ld lv lval = 
    match h with
      (* Formulas coming from simplification of slide without knowing the sign of the slide *)
      (* ((π'_v^i = 0.) ∨ (π'_v^i = 1.)) *)
      | HOp(
          Or,
          HAtom(Eq, PiExit(v1,i1), HConstant(fl1)), 
          HAtom(Eq, PiExit(v2,i2), HConstant(fl2))) -> 
        let sol1 = (cstr_in_hcl_with_eq hcl (PiExit(v1,i1)) fl1) in
        let sol2 = (cstr_in_hcl_with_eq hcl (PiExit(v2,i2)) fl2) in
        let sol1_1 = (in_lval (PiExit(v1,i1)) fl1 lval) in
        let sol2_1 = (in_lval (PiExit(v2,i2)) fl2 lval) in
        if (sol1 = "exist" || sol1_1 || sol2 = "exist" || sol2_1)
        then (lv, (lc,ld))
        else (
          if (sol1 = "opposite")
          then ([(PiExit(v2,i2), fl2)] @ lv, (lc, ld))
          else (
            if (sol2 = "opposite")
            then ([(PiExit(v1,i1), fl1)] @ lv, (lc, ld))
            else (lv, (lc, [h] @ ld))))
      (* ((π'_v^i = fl) ∨ ((π'_v^i = fl) ∧ π_v^i=π'_v^i − C_v,w,k × Δt))
      OR ((π'_v^i = fl) ∨ [(π'_v^i = fl) ∧ ((C_v,w,k comp 0.) ∨ π_v^i=π'_v^i − C_v,w,k × Δt))] *)
      | HOp(
          Or,
          HAtom(Eq, PiExit(v1,i1), HConstant(fl1)),
          HOp(
            And, 
            HAtom(Eq, PiExit(v2,i2), HConstant(fl2)), 
            ht1)) -> 
        let sol1 = (cstr_in_hcl_with_eq hcl (PiExit(v1,i1)) fl1) in
        let sol2 = (cstr_in_hcl_with_eq hcl (PiExit(v2,i2)) fl2) in
        let sol1_1 = (in_lval (PiExit(v1,i1)) fl1 lval) in
        let sol2_1 = (in_lval (PiExit(v2,i2)) fl2 lval) in
        if (sol1 = "exist" || sol1_1) 
        then (lv, (lc, ld)) 
        else (
          if (sol1 = "opposite")
          then (
            let op = contain_logical_connective ht1 in
            if (op = "Or")
            then ([(PiExit(v2,i2), fl2)] @ lv, (lc, [ht1] @ ld))
            else (
              if (op = "And")
              then ([(PiExit(v2,i2), fl2)] @ lv, ((in_lc ht1 lc), ld))
              else ([(PiExit(v2,i2), fl2)] @ lv, ([ht1] @ lc, ld))))
          else (
            if (sol2 = "exist" || sol2_1)
            then (lv, (lc, [HOp(Or, HAtom(Eq, PiExit(v1,i1), HConstant(fl1)), ht1)] @ ld))
            else (
              if (sol2 = "opposite")
              then ([(PiExit(v1,i1), fl1)] @ lv, (lc, ld))
              else (lv, (lc, [h] @ ld)))))
      (* ((π'_v^i = fl ∧ π_v^i=π'_v^i − C_v,w,k × Δt)                     ∨ (π'_v^i = fl ∧ π_v^i=π'_v^i − C_v,w,k × Δt))
      OR ((π'_v^i = fl ∧ π_v^i=π'_v^i − C_v,w,k × Δt)                     ∨ (π'_v^i = fl ∧ (C_v,w,k comp 0. ∨ π_v^i=π'_v^i − C_v,w,k × Δt)))
      OR ((π'_v^i = fl ∧ (C_v,w,k comp 0. ∨ π_v^i=π'_v^i − C_v,w,k × Δt)) ∨ (π'_v^i = fl ∧ π_v^i=π'_v^i − C_v,w,k × Δt))
      OR ((π'_v^i = fl ∧ (C_v,w,k comp 0. ∨ π_v^i=π'_v^i − C_v,w,k × Δt)) ∨ (π'_v^i = fl ∧ (C_v,w,k comp 0. ∨ π_v^i=π'_v^i − C_v,w,k × Δt))) *)
      | HOp(
          Or,
          HOp(
            And,
            HAtom(Eq, PiExit(v1,i1), HConstant(fl1)),
            ht1),
          HOp(
            And, 
            HAtom(Eq, PiExit(v2,i2), HConstant(fl2)), 
            ht2)) -> 
        let sol1 = (cstr_in_hcl_with_eq hcl (PiExit(v1,i1)) fl1) in
        let sol2 = (cstr_in_hcl_with_eq hcl (PiExit(v2,i2)) fl2) in
        let sol1_1 = (in_lval (PiExit(v1,i1)) fl1 lval) in
        let sol2_1 = (in_lval (PiExit(v2,i2)) fl2 lval) in
        let op1 = contain_logical_connective ht1 in
        let op2 = contain_logical_connective ht2 in
        if (sol1 = "exist" || sol1_1) 
        then (
          if (op1 = "Or")
          then (lv, (lc, [ht1] @ ld))
          else (
            if (op1 = "And")
            then (lv, (in_lc ht1 lc, ld))
            else (lv, ([ht1] @ lc, ld)))
          )
        else (
          if (sol1 = "opposite")
          then (
            if (op2 = "Or")
            then ([(PiExit(v2,i2), fl2)] @ lv, (lc, [ht2] @ ld))
            else (
              if (op2 = "And")
              then ([(PiExit(v2,i2), fl2)] @ lv, ((in_lc ht2 lc), ld))
              else ([(PiExit(v2,i2), fl2)] @ lv, ([ht2] @ lc, ld))))
          else (
            if (sol2 = "exist" || sol2_1)
            then (
              if (op2 = "Or")
              then (lv, (lc, [ht2] @ ld))
              else (
                if (op2 = "And")
                then (lv, (in_lc ht2 lc, ld))
                else (lv, ([ht2] @ lc, ld))
                )
              )
            else (
              if (sol2 = "opposite")
              then (
                let op = contain_logical_connective ht1 in
                if (op = "Or")
                then ([(PiExit(v1,i1), fl1)] @ lv, (lc, [ht1] @ ld))
                else (
                  if (op = "And")
                  then ([(PiExit(v1,i1), fl1)] @ lv, (in_lc ht1 lc, ld))
                  else ([(PiExit(v1,i1), fl1)] @ lv, ([ht1] @ lc, ld))))
              else (lv, (lc, [h] @ ld)))))
      (* The next pattern comes from a result of the previous pattern *)
      (* ([(π'_v^i = fl) ∧ π_v^i=π'_v^i − C_v,w,k × Δt)]                       ∨ (π_v^i=π'_v^i − C_v,w,k × Δt))
      OR ([(π'_v^i = fl) ∧ π_v^i=π'_v^i − C_v,w,k × Δt)]                       ∨ [(C_v,w,k comp 0.)  ∨ (π_v^i=π'_v^i − C_v,w,k × Δt)]
      OR ([(π'_v^i = fl) ∧ ((C_v,w,k comp 0.) ∨ π_v^i=π'_v^i − C_v,w,k × Δt))] ∨ (π_v^i=π'_v^i − C_v,w,k × Δt))
      OR ([(π'_v^i = fl) ∧ ((C_v,w,k comp 0.) ∨ π_v^i=π'_v^i − C_v,w,k × Δt))] ∨ [((C_v,w,k comp 0.) ∨ π_v^i=π'_v^i − C_v,w,k × Δt))] *)
      | HOp(
          Or,
          HOp(
            And, 
            HAtom(Eq, PiExit(v1,i1), HConstant(fl1)), 
            ht1),
          ht2) -> 
        let sol1 = (cstr_in_hcl_with_eq hcl (PiExit(v1,i1)) fl1) in
        let sol1_1 = (in_lval (PiExit(v1,i1)) fl1 lval) in
        if (sol1 = "exist" || sol1_1) 
        then (lv, (lc, [HOp(Or, ht1, ht2)] @ ld)) 
        else (
          if (sol1 = "opposite")
          then (
            let op = contain_logical_connective ht1 in
            if (op = "Or")
            then (lv, (lc, [ht2] @ ld))
            else (
              if (op = "And")
              then (lv, ((in_lc ht2 lc), ld))
              else (lv, ([ht2] @ lc, ld))))
          else (lv, (lc, [h] @ ld)))
      (* ((π'_v^i = 0 ∨ π'_v^i = 1) ∨ (π_v^i=π'_v^i − C_v,w,k × Δt)) *)
      | HOp(
          Or,
          HOp(
            Or,
            HAtom(Eq, PiExit(v0,i0), HConstant(0.)),
            HAtom(Eq, PiExit(v1,i1), HConstant(1.))),
          ha2) ->
        let sol0 = (cstr_in_hcl_with_eq hcl (PiExit(v0,i0)) 0.) in
        let sol1 = (cstr_in_hcl_with_eq hcl (PiExit(v1,i1)) 1.) in
        let sol0_1 = (in_lval (PiExit(v0,i0)) 0. lval) in
        let sol0_2 = (in_lval (PiExit(v1,i1)) 1. lval) in
        if (sol0 = "exist" || sol1 = "exist" || sol0_1 || sol0_2)
        then (lv, (lc,ld))
        else (
          if (sol0 = "opposite")
          then (
            if (sol1 = "opposite")
            then (lv, ([ha2] @ lc, ld))
            else (lv, (lc, [HOp(Or, HAtom(Eq, PiExit(v1,i1), HConstant(1.)), ha2)] @ ld)))
          else (
            if (sol1 = "opposite")
            then (lv, (lc, [HOp(Or, HAtom(Eq, PiExit(v0,i0), HConstant(0.)), ha2)] @ ld))
            else (lv, (lc, [h] @ ld))))
      (* ((π'_v^i ≠ fl) ∨ ((C_v,w,k comp 0.) ∧ π_v^i=π'_v^i − C_v,w,k × Δt)) *)
      | HOp(Or, HAtom(NEq, PiExit(v,i), HConstant(fl)), HOp(And, ht1, ht2)) -> 
        let sol = (in_lval_neq (PiExit(v,i)) fl lval) in
        let sol2 = (cstr_in_hcl_with_neq hcl (PiExit(v,i)) fl) in
        if (sol = "exist" || sol2 = "exist") 
        then (lv, (lc,ld)) 
        else (
          if (sol = "opposite" || sol2 = "opposite")
          then (lv, ([ht1] @ ([ht2] @ lc), ld))
          else (lv, (lc, [h] @ ld)))
      (* ((π'_v^i = fl) ∨ (π_v^i=π'_v^i − C_v,w,k × Δt)) *)
      | HOp(
          Or,
          HAtom(Eq, PiExit(v,i), HConstant(fl)), 
          ha2) -> 
        let sol = (cstr_in_hcl_with_eq hcl (PiExit(v,i)) fl) in
        let sol2 = (in_lval (PiExit(v,i)) fl lval) in
        if (sol = "exist" || sol2)
        then (lv, (lc,ld))
        else (
          if (sol = "opposite")
          then (
            let op = contain_logical_connective ha2 in
            if (op = "Or")
            then (lv, (lc, [ha2] @ ld))
            else (
              if (op = "And")
              then (lv, ((in_lc ha2 lc), ld))
              else (lv, ([ha2] @ lc, ld))))
          else (lv, (lc, [h] @ ld)))
      (* The next pattern let in hDisj the pattern coming from F(Delta t) : 
      (Cv>0 OR  Pi comp Pi' - Cv * Delta t ) OR (C comp fl OR C comp fl) *)
      | HOp(Or, h1, h2) ->  (lv, (lc, ([HOp(Or, h1, h2)] @ ld)))
      | other -> raise(Invalid_argument("find_cstr: Error\n" ^ (string_of_hybridCondition other) ))
    in  
  (** Reversed list of HybridDisjunction for fixpoint computation **)
  let rec reversed_list l lf =
    match l with
      | [] -> lf
      | h::[] -> [h] @ lf
      | h::t -> reversed_list t ([h] @ lf)
    in
  let rec hd_list hcl hdl lc ld lv lval = (* Assumes all element in the list *)
    match hdl with
      | [] -> raise(Invalid_argument "hd_list: Error")
      | h :: [] -> let res = find_cstr hcl h lc ld lv lval in 
        ((fst res) @ lval, ((fst (snd res)) @ hcl, (reversed_list (snd (snd res)) [])))
      | h :: t -> let res = find_cstr hcl h lc ld lv lval in 
        hd_list hcl t (fst (snd res)) (snd (snd res)) (fst res) lval
    in
  let get_tuple s =
    match s with
      | (lval, cstr) -> 
        let res = (hd_list (fst cstr) (snd cstr) [] [] [] lval) in
        (fst res, snd res)
    in
  let rec eval_all_element s l =
    match s with
      | [] -> raise(Invalid_argument "eval_all_element: list sval empty")
      | h :: [] -> [get_tuple h] @ l
      | h :: t -> eval_all_element t ([get_tuple h] @ l)
    in eval_all_element sval []
;;


(* The function below determines if some constraints of celerities (included in 
"disjunctive formula") exist. We search the current constraint in the 
hybridConjunction and lvaluation lists. If it doesn't exist, we look for the 
neighbor celerity to simplify. If we can't have some information with the 
neighbour celerity, we let the formula in the "disjunctive formula" list *)
let simplify_cstr_cel sval =
  let same_comp_neighbor c1 c2 = (* Compare between the celerity and its neighbor both comparators *)
      if (((c1 = GE) && (c2 = GT)) || 
          ((c1 = GE) && (c2 = GE)) || 
          ((c1 = LE) && (c2 = LT)) || 
          ((c1 = LE) && (c2 = LE)) ||
          ((c1 = Eq) && (c2 = Eq)))
      then ("exist")
    else if (((c1 = GT) && (c2 = LT)) || 
             ((c1 = GT) && (c2 = LE)) || 
             ((c1 = LT) && (c2 = GT)) || 
             ((c1 = LT) && (c2 = GE)))
    then ("opposite")
  else (
    if ( (c1 = NEq) && ((c2 = GT) || (c2 = LT) || (c2 = NEq))) 
    then ("exist") 
    else if ((c1 = NEq) && (c2 = Eq))
      then ("opposite")
      else ("notexist"))
    in
  let same_comp c1 c2 = (* Compare between two formula of the same celerity their comparators. *)
      if (((c1 = GT)  && (c2 = GT)) || 
          ((c1 = LT)  && (c2 = LT)) || 
          ((c1 = GE)  && (c2 = GT)) || 
          ((c1 = GE)  && (c2 = GE)) || 
          ((c1 = LE)  && (c2 = LT)) || 
          ((c1 = LE)  && (c2 = LE)) ||
          ((c1 = NEq) && (c2 = GT)) || (* This case means we found C > 0 in hCOnj for the constraint C ≠ 0 *) 
          ((c1 = NEq) && (c2 = LT)) || (* This case means we found C < 0 in hCOnj for the constraint C ≠ 0 *) 
          ((c1 = Eq)  && (c2 = Eq)))
      then ("exist")
    else if (((c1 = GT)  && (c2 = LT)) || 
             ((c1 = GT)  && (c2 = LE)) || 
             ((c1 = LT)  && (c2 = GT)) || 
             ((c1 = LT)  && (c2 = GE)) || 
             ((c1 = GE)  && (c2 = LT)) || 
             ((c1 = LE)  && (c2 = GT)) ||
             ((c1 = NEq) && (c2 = Eq)))
    then ("opposite")
  else (("notexist"))
    in
  let rec find_neighbor_in_lval lval cel c1 = 
  (* Search the neighbor celerity cel of the current one in the valulation list lval. 
    c1 is the comparator of the "disjunctive formula" of the current celerity *)
    match lval with
      | [] -> raise(Invalid_argument "find_neighbor_in_lval: lval list empty")
      | h :: [] -> (match h with
        | (ht, f) -> 
            if (ht = cel) 
            then (
              if (f > 0.)
              then (same_comp_neighbor c1 GT)
              else (
                if f < 0.
                then (same_comp_neighbor c1 LT)
                else (
                  if (f = 0.)
                  then (same_comp_neighbor c1 Eq)
                  else ("notexist"))))
            else "notexist")
      | h :: t -> (match h with
        | (ht, f) -> 
            if (ht = cel) 
            then (
              if (f > 0.)
              then (same_comp_neighbor c1 GT)
              else (
                if f < 0.
                then (same_comp_neighbor c1 LT) 
                else ("notexist")))
            else (find_neighbor_in_lval t cel c1))
    in
  let rec find_cel_in_lval lval cel c1 = (* Look for the neighbor celerity cel of the current one in the valulation list lval. c1 is the comparator of the "disjunctive formula" of the current celerity *)
    match lval with
      | [] -> raise(Invalid_argument "find_cel_in_lval: lval list empty")
      | h :: [] -> (match h with
        | (ht, f) -> 
            if (ht = cel) 
            then (
              if (f > 0.)
              then (same_comp c1 GT)
              else (
                if f < 0.
                then (same_comp c1 LT)
                else (
                  if (f = 0.)
                  then (same_comp c1 Eq)
                  else ("notexist"))))
            else "notexist")
      | h :: t -> (match h with
        | (ht, f) -> 
            if (ht = cel) 
            then (
              if (f > 0.)
              then (same_comp c1 GT)
              else (
                if f < 0.
                then (same_comp c1 LT) 
                else ("notexist")))
            else (find_cel_in_lval t cel c1))
    in
  let rec find_cstr_for_each_neighbor hcl cel c1 = 
    (* Find a constraint of the Celerity cel in the hybridConjonction list,
     and compare its comparator with the comparator c1 *)
    match hcl with 
      | [] -> raise(Invalid_argument "find_cstr_for_each_neighbor: hybridConjunction list empty")
      | h :: [] -> (match h with
        | HAtom(c2, ht1, ht2) -> if (ht1 = cel) then (same_comp_neighbor c1 c2) else ("notexist")
        | _ -> raise(Invalid_argument "find_cstr_for_each_neighbor: error"))
      | h :: t -> (match h with
        | HAtom(c2, ht1, ht2) -> if (ht1 = cel) then (same_comp_neighbor c1 c2) else (find_cstr_for_each_neighbor t cel c1)
        | _ -> raise(Invalid_argument "find_cstr_for_each_neighbor: error"))
    in
  let rec eval_each_neighbor l_neighbor hcl =
    match l_neighbor with
      | [] -> raise(Invalid_argument "eval_each_neighbor: should not be empty")
      | h::[] -> find_cstr_for_each_neighbor hcl (fst h) (snd h)
      | h::t -> 
        let sol = (find_cstr_for_each_neighbor hcl (fst h) (snd h)) in
          if (sol = "exist" || sol = "opposite")
          then (sol)
          else (eval_each_neighbor t hcl)
    in
  let rec eval_each_neighbor_in_lval l_neighbor lval =
    match l_neighbor with
      | [] -> raise(Invalid_argument "eval_each_neighbor_in_lval: should not be empty")
      | h::[] -> find_neighbor_in_lval lval (fst h) (snd h)
      | h::t -> 
        let sol = (find_neighbor_in_lval lval (fst h) (snd h)) in
          if (sol = "exist" || sol = "opposite")
          then (sol)
          else (eval_each_neighbor_in_lval t lval)
    in
  let search_neighbor h = (* Determine the neighbor celerity *)
    match h with
      | HAtom(comp, ht1, ht2) -> (match ht1 with
              | Celerity(v, w, n) -> 
                if (n = 0) 
                then [(Celerity(v, w, (n+1)), comp)] 
                else if (n = (getbound(v)))
                then [(Celerity(v, w, (n-1)), comp)]
                else [(Celerity(v, w, (n-1)), comp) ; (Celerity(v, w, (n+1)), comp)] 
              | _ -> raise(Invalid_argument ("search_neighbor: only Celerity compared with 0 :\n" ^ (string_of_hybridCondition h))))
      | _ -> raise(Invalid_argument "search_neighbor: Error")
    in
  let rec get_hterm h = (* Give the hybridCondition ht1 of the formula *)
    match h with 
      | HAtom(comp, ht1, ht2) -> ht1
      | _ -> raise(Invalid_argument ("get_hterm: Error\n" ^ (string_of_hybridCondition h)))
    in
  let rec get_comp h = (* Give the comparator of the hybridCondition HAtom*)
    match h with
      | HAtom(comp, ht1, ht2) -> comp
      | _ -> raise(Invalid_argument "get_comp: Error")
    in
  let rec exist_cstr ht1 c1 hcl = (* Give the result of both constraints of there exists a constraint of the current celerity in the hybridCondition list hcl *)
    match hcl with
    | [] -> raise(Invalid_argument "exist_cstr: hybridConjunction list empty")
    | hd :: [] -> if ((get_hterm hd) = ht1) then (same_comp c1 (get_comp hd)) else ("notexist")
    | hd :: tl -> if ((get_hterm hd) = ht1) then (same_comp c1 (get_comp hd)) else (exist_cstr ht1 c1 tl)
    in
  let rec find_all_cstr hcl h lc ld lval h1 op h2 =
    match h with
      | HAtom(comp, ht1, ht2) -> 
        let assess_cstr = exist_cstr ht1 comp hcl in 
        let eval_neighbor = eval_each_neighbor (search_neighbor (HAtom(comp, ht1, ht2))) hcl in
        let eval_neighbor_in_lval = eval_each_neighbor_in_lval (search_neighbor (HAtom(comp, ht1, ht2))) lval in
          if (assess_cstr = "exist")
          then (lc, ld)
          else 
            if (assess_cstr = "opposite")
            then ([h1] @ lc, ld)
            else
              if (eval_neighbor = "exist")
              then (lc, ld)
              else
                if (eval_neighbor = "opposite")
                then ([h1] @ lc, ld)
                else
                  if (eval_neighbor_in_lval = "exist")
                  then (lc, ld)
                  else
                    if (eval_neighbor_in_lval = "opposite")
                    then ([h1] @ lc, ld)
                  else (lc, ([HOp(op, h1, h2)] @ ld))
      | HOp(op, ht1, ht2) ->
      let assess_cstr = exist_cstr (get_hterm ht1) (get_comp ht1) hcl in 
      let eval_neighbor = eval_each_neighbor (search_neighbor ht1) hcl in
      let eval_neighbor_in_lval = eval_each_neighbor_in_lval (search_neighbor ht1) lval in
        if (assess_cstr = "exist")
        then (lc, ld)
        else
          if (assess_cstr = "opposite")
          then ([h1] @ lc, ld)
          else
            if (eval_neighbor = "exist")
            then (lc, ld)
            else
              if (eval_neighbor = "opposite")
              then ([h1] @ lc, ld)
              else
                if (eval_neighbor_in_lval = "exist")
                then (lc, ld)
                else
                  if (eval_neighbor_in_lval = "opposite")
                  then ([h1] @ lc, ld)
                  else find_all_cstr hcl ht2 lc ld lval h1 op h2
      | _ -> raise(Invalid_argument "find_all_cstr: Should be of the form C_{v,w,k<n} > 0 ∧ C_{v,w,k>n} < 0")
    in
  let find_cstr hcl h lc ld lval = 
  (* Main function which assesses if there exists a constraint 
  of the current celerity in the hybridCondition list hcl, 
  else if there exists a constraint for its neighbor celerity, 
  and put/remove/keep constraint in hybridCondition list 
  with or without "disjunctive formula" consequently *)
  (* Gère (C_v,w,k comp 0. ∨ C_v,w',k' < 0.) et
  (C_v,w,k comp 0. ∨ π_v^i≤π'_v^i − C_v,w,k × Δt) *)
    match h with
      | HOp(op, h1, h2) ->
      let assess_cstr = exist_cstr (get_hterm h1) (get_comp h1) hcl in
      let eval_neighbor = eval_each_neighbor (search_neighbor h1) hcl in
      let eval_neighbor_in_lval = eval_each_neighbor_in_lval (search_neighbor h1) lval in
        if (assess_cstr = "exist")
        then (lc, ld)
        else
          if (assess_cstr = "opposite")
          then ([h2] @ lc, ld)
          else
            if (eval_neighbor = "exist")
            then (lc, ld)
            else
              if (eval_neighbor = "opposite")
              then ([h2] @ lc, ld)
              else
                if (eval_neighbor_in_lval = "exist")
                then (lc, ld)
                else
                  if (eval_neighbor_in_lval = "opposite")
                  then ([h2] @ lc, ld)
                  else
                    if ((get_comp h1) = NEq)
                    then (match h2 with
                      | HAtom(comp, Celerity(v,w,k), HConstant(fl)) -> find_all_cstr hcl h2 lc ld lval h1 op h2
                      | _ -> (lc, ([HOp(op, h1, h2)] @ ld)))
                    else (lc, ([HOp(op, h1, h2)] @ ld))
      | other -> raise(Invalid_argument "find_cstr: Error")
    in
  let eval_cel h =
    match h with
      | HAtom(comp, Celerity(v,w,k), fl) -> Celerity(v,w,k)
      | _ -> raise(Invalid_argument ("eval_cel : should be a celerity and we found " ^ (string_of_hybridCondition h)))
  in
  (* 
  The constraints analysed in the below function are of the form :
  ((π'_v^i ≠ fl) ∨ ((C_v,w,k comp 0.) ∧ (π_v^i=π'_v^i − C_v,w,k × Δt))) avec

    h = (C_v,w,k comp 0.)
    h2 = (π'_v^i ≠ fl) ∨ (π_v^i=π'_v^i − C_v,w,k × Δt)
    h3 = (π'_v^i ≠ fl)
    h_initial = ((π'_v^i ≠ fl) ∨ ((C_v,w,k comp 0.) ∧ (π_v^i=π'_v^i − C_v,w,k × Δt)))

  *)
  let find_cstr_one_cel hcl h lc ld lval h2 h3 h_initial =
    let c_neighbor = (search_neighbor h) in
    let comp = get_comp h in
    let exist_c = exist_cstr (get_hterm h) comp hcl in 
    let exist_neighbor = eval_each_neighbor c_neighbor hcl in
    let exist_neighbor_in_lval = eval_each_neighbor_in_lval c_neighbor lval in
    let exist_c_in_lval = find_cel_in_lval lval (eval_cel h) comp in
    if 
      (exist_c = "exist") || 
      (exist_neighbor = "exist") || 
      (exist_c_in_lval = "exist") || 
      (exist_neighbor_in_lval = "exist")
    then (lc, [h2] @ ld)
    else if 
      (exist_c = "opposite") || 
      (exist_neighbor = "opposite") || 
      (exist_c_in_lval = "opposite") || 
      (exist_neighbor_in_lval = "opposite")
    then ([h3] @ lc, ld)
    else (lc, [h_initial] @ ld)
  in
  let find_cstr_two_cel_final hcl h lc ld lval h2 h_final =
    let c_neighbor = (search_neighbor h) in
    let comp = get_comp h in
    let exist_c = exist_cstr (get_hterm h) comp hcl in 
    let exist_neighbor = eval_each_neighbor c_neighbor hcl in
    let exist_neighbor_in_lval = eval_each_neighbor_in_lval c_neighbor lval in
    let exist_c_in_lval = find_cel_in_lval lval (eval_cel h) comp in
    if 
      (exist_c = "exist") || 
      (exist_neighbor = "exist") || 
      (exist_c_in_lval = "exist") || 
      (exist_neighbor_in_lval = "exist")
    then (lc, ld)
    else if 
      (exist_c = "opposite") || 
      (exist_neighbor = "opposite") || 
      (exist_c_in_lval = "opposite") || 
      (exist_neighbor_in_lval = "opposite")
    then ([h2] @ lc,ld)
    else (lc, [h_final] @ ld)
  in
  let find_cstr_two_cel hcl h lc ld lval h2 h3 h2Uh3 h_initial =
    let c_neighbor = (search_neighbor h) in
    let comp = get_comp h in
    let exist_c = exist_cstr (get_hterm h) comp hcl in 
    let exist_neighbor = eval_each_neighbor c_neighbor hcl in
    let exist_neighbor_in_lval = eval_each_neighbor_in_lval c_neighbor lval in
    let exist_c_in_lval = find_cel_in_lval lval (eval_cel h) comp in
    if 
      (exist_c = "exist") || 
      (exist_neighbor = "exist") || 
      (exist_c_in_lval = "exist") || 
      (exist_neighbor_in_lval = "exist")
    then (lc, ld)
    else if 
      (exist_c = "opposite") || 
      (exist_neighbor = "opposite") || 
      (exist_c_in_lval = "opposite") || 
      (exist_neighbor_in_lval = "opposite")
    then (find_cstr_two_cel_final hcl h3 lc ld lval h2 h2Uh3)
    else (find_cstr_two_cel_final hcl h3 lc ld lval (HOp(Or, h, h2)) h_initial)
  in
  (* 
  Simplify constraint of the form
  (A ∨ B) ∨ (C ∧ D)
  with 
  A : C_v,w,k comp 0.
  B : π_v^i≤π'_v^i − C_v,w,k × Δt
  C : C_v,w',k' comp 0.
  D : C_v,w'',k'' comp 0.
  A was computed and find in hConj so removed from the formula
  C was computed and can be find in hConj/removed (cpt=0) or not find/,ot removed (cpt=1)
  *)
  let find_cstr_one_cel_with_conj hcl h lc ld lval h2 h3 cpt =
    let c_neighbor = (search_neighbor h) in
    let comp = get_comp h in
    let exist_c = exist_cstr (get_hterm h) comp hcl in 
    let exist_neighbor = eval_each_neighbor c_neighbor hcl in
    let exist_neighbor_in_lval = eval_each_neighbor_in_lval c_neighbor lval in
    let exist_c_in_lval = find_cel_in_lval lval (eval_cel h) comp in
    if 
      (exist_c = "exist") || 
      (exist_neighbor = "exist") || 
      (exist_c_in_lval = "exist") || 
      (exist_neighbor_in_lval = "exist")
    then (lc, ld)
    else if 
      (exist_c = "opposite") || 
      (exist_neighbor = "opposite") || 
      (exist_c_in_lval = "opposite") || 
      (exist_neighbor_in_lval = "opposite")
    then ([h2] @ lc, ld)
    else (
      if (cpt = 0)
      then (lc, [HOp(Or, h2, h)] @ ld)
      else (lc, [HOp(Or, h2, HOp(Or, h3, h))] @ ld))
  in
  (* 
  Simplify constraint of the form
  (A ∨ B) ∨ (C ∧ D)
  with 
  A : C_v,w,k comp 0.
  B : π_v^i≤π'_v^i − C_v,w,k × Δt
  C : C_v,w',k' comp 0.
  D : C_v,w'',k'' comp 0.
  A was computed and find in hConj so removed from the formula
  *)
  let find_cstr_two_cel_with_conj hcl h lc ld lval h2 h3 =
    let c_neighbor = (search_neighbor h) in
    let comp = get_comp h in
    let exist_c = exist_cstr (get_hterm h) comp hcl in 
    let exist_neighbor = eval_each_neighbor c_neighbor hcl in
    let exist_neighbor_in_lval = eval_each_neighbor_in_lval c_neighbor lval in
    let exist_c_in_lval = find_cel_in_lval lval (eval_cel h) comp in
    if 
      (exist_c = "exist") || 
      (exist_neighbor = "exist") || 
      (exist_c_in_lval = "exist") || 
      (exist_neighbor_in_lval = "exist")
    then (find_cstr_one_cel_with_conj hcl h3 lc ld lval h2 h 0)
    else if 
      (exist_c = "opposite") || 
      (exist_neighbor = "opposite") || 
      (exist_c_in_lval = "opposite") || 
      (exist_neighbor_in_lval = "opposite")
    then ([h2] @ lc, ld)
    else (find_cstr_one_cel_with_conj hcl h3 lc ld lval h2 h 1)
  in
  (* 
  Simplify constraint of the form
  (A ∨ B) ∨ (C ∧ h)
  with 
  A : C_v,w,k comp 0.
  B : π_v^i≤π'_v^i − C_v,w,k × Δt
  C : C_v,w',k' comp 0.
  h : C_v,w'',k'' comp 0.
  A was computed and not find in hConj
  C was computed and can be find in hConj/removed (cpt=0) or not find/,ot removed (cpt=1)
  *)
  let find_cstr_one_cel_with_conj_keep hcl h lc ld lval h2 h3 h4 cpt =
    let c_neighbor = (search_neighbor h) in
    let comp = get_comp h in
    let exist_c = exist_cstr (get_hterm h) comp hcl in 
    let exist_neighbor = eval_each_neighbor c_neighbor hcl in
    let exist_neighbor_in_lval = eval_each_neighbor_in_lval c_neighbor lval in
    let exist_c_in_lval = find_cel_in_lval lval (eval_cel h) comp in
    if 
      (exist_c = "exist") || 
      (exist_neighbor = "exist") || 
      (exist_c_in_lval = "exist") || 
      (exist_neighbor_in_lval = "exist")
    then (lc, ld)
    else if 
      (exist_c = "opposite") || 
      (exist_neighbor = "opposite") || 
      (exist_c_in_lval = "opposite") || 
      (exist_neighbor_in_lval = "opposite")
    then (lc, [HOp(Or, h3, h2)] @ ld)
    else (
      if (cpt = 0)
      then (lc, [HOp(Or, HOp(Or, h3, h2), h)] @ ld)
      else (lc, [HOp(Or, HOp(Or, h3, h2), HOp(And, h4, h))] @ ld))
  in
  (* 
  Simplify constraint of the form
  (A ∨ B) ∨ (h ∧ h3)
  with 
  A : C_v,w,k comp 0.
  B : π_v^i≤π'_v^i − C_v,w,k × Δt
  h : C_v,w',k' comp 0.
  h3 : C_v,w'',k'' comp 0.
  A was computed and not find in hConj
  *)
  let find_cstr_two_cel_with_conj_keep hcl h lc ld lval h2 h3 h4 =
    let c_neighbor = (search_neighbor h) in
    let comp = get_comp h in
    let exist_c = exist_cstr (get_hterm h) comp hcl in 
    let exist_neighbor = eval_each_neighbor c_neighbor hcl in
    let exist_neighbor_in_lval = eval_each_neighbor_in_lval c_neighbor lval in
    let exist_c_in_lval = find_cel_in_lval lval (eval_cel h) comp in
    if 
      (exist_c = "exist") || 
      (exist_neighbor = "exist") || 
      (exist_c_in_lval = "exist") || 
      (exist_neighbor_in_lval = "exist")
    then (find_cstr_one_cel_with_conj_keep hcl h3 lc ld lval h2 h4 h 0)
    else if 
      (exist_c = "opposite") || 
      (exist_neighbor = "opposite") || 
      (exist_c_in_lval = "opposite") || 
      (exist_neighbor_in_lval = "opposite")
    then (lc, [HOp(Or, h4, h2)] @ ld)
    else (find_cstr_one_cel_with_conj_keep hcl h3 lc ld lval h2 h4 h 1)
  in
  (* 
  Constraint of the form
  (h ∨ h2) ∨ (h3 ∧ h4)
  with 
  h : C_v,w,k comp 0.
  h2 : π_v^i≤π'_v^i − C_v,w,k × Δt
  h3 : C_v,w',k' comp 0.
  h4 : C_v,w'',k'' comp 0.
  *)
  let find_cstr_three_cel hcl h lc ld lval h2 h3 h4 =
    let c_neighbor = (search_neighbor h) in
    let comp = get_comp h in
    let exist_c = exist_cstr (get_hterm h) comp hcl in 
    let exist_neighbor = eval_each_neighbor c_neighbor hcl in
    let exist_neighbor_in_lval = eval_each_neighbor_in_lval c_neighbor lval in
    let exist_c_in_lval = find_cel_in_lval lval (eval_cel h) comp in
    if 
      (exist_c = "exist") || 
      (exist_neighbor = "exist") || 
      (exist_c_in_lval = "exist") || 
      (exist_neighbor_in_lval = "exist")
    then (lc, ld)
    else if 
      (exist_c = "opposite") || 
      (exist_neighbor = "opposite") || 
      (exist_c_in_lval = "opposite") || 
      (exist_neighbor_in_lval = "opposite")
    then (find_cstr_two_cel_with_conj hcl h3 lc ld lval h2 h4)
    else (find_cstr_two_cel_with_conj_keep hcl h3 lc ld lval h2 h4 h)
  in
  (* 
    Constraint of the form
    [π'_v^i= 0 ∧ (C_v,w,k ≥ 0. ∨ π_v^i<π'_v^i − C_v,w,k × Δt] ∨ 
    [π'_v^i= 1 ∧ (C_v,w,k ≤ 0. ∨ π_v^i>π'_v^i − C_v,w,k × Δt]
    = [ht1 ∧ (cel2 ∨ ht3] ∨ [ht4 ∧ (cel5 ∨ ht6]
    with
  ht1  = π'_v^i= 0
  cel2 = C_v,w,k ≥ 0.
  ht3  = π_v^i<π'_v^i − C_v,w,k × Δt
  ht4  = π'_v^i= 1
  cel5  = C_v,w,k ≤ 0.
  ht6  = π_v^i>π'_v^i − C_v,w,k × Δt

  ht_prev depends on the previous function. It can be :
  ht1 if cel1 is True
  HOp(And, ht1, ht3) if cel1 is False
  HOp(And, ht1, HOp(Or, cel2, ht3)) if cel1 is not known
  *)
  let find_cstr_sl_final_exist hcl cel5 lc ld lval ht4 ht6 ht1 =
    let c_neighbor = (search_neighbor cel5) in
    let comp = get_comp cel5 in
    let exist_c = exist_cstr (get_hterm cel5) comp hcl in 
    let exist_neighbor = eval_each_neighbor c_neighbor hcl in
    let exist_neighbor_in_lval = eval_each_neighbor_in_lval c_neighbor lval in
    let exist_c_in_lval = find_cel_in_lval lval (eval_cel cel5) comp in
    if 
      (exist_c = "exist") || 
      (exist_neighbor = "exist") || 
      (exist_c_in_lval = "exist") || 
      (exist_neighbor_in_lval = "exist")
    then (lc, [HOp(Or, ht1, ht4)] @ ld)
    else (
      if (
        (exist_c = "opposite") || 
        (exist_neighbor = "opposite") || 
        (exist_c_in_lval = "opposite") || 
        (exist_neighbor_in_lval = "opposite"))
    then (lc, [HOp(Or, ht1, HOp(And, ht4, ht6))] @ ld)
    else (lc, [HOp(Or, ht1, HOp(And, ht4, HOp(Or, cel5, ht6)))] @ ld))
  in
  let find_cstr_sl_final_opp hcl cel5 lc ld lval ht4 ht6 ht1_and_ht3 =
    let c_neighbor = (search_neighbor cel5) in
    let comp = get_comp cel5 in
    let exist_c = exist_cstr (get_hterm cel5) comp hcl in 
    let exist_neighbor = eval_each_neighbor c_neighbor hcl in
    let exist_neighbor_in_lval = eval_each_neighbor_in_lval c_neighbor lval in
    let exist_c_in_lval = find_cel_in_lval lval (eval_cel cel5) comp in
    if 
      (exist_c = "exist") || 
      (exist_neighbor = "exist") || 
      (exist_c_in_lval = "exist") || 
      (exist_neighbor_in_lval = "exist")
    then (lc, [HOp(Or, ht4, ht1_and_ht3)] @ ld)
    else (
      if (
        (exist_c = "opposite") || 
        (exist_neighbor = "opposite") || 
        (exist_c_in_lval = "opposite") || 
        (exist_neighbor_in_lval = "opposite"))
    then (lc, [HOp(Or, ht1_and_ht3, HOp(And, ht4, ht6))] @ ld)
    else (lc, [HOp(Or, ht1_and_ht3, HOp(And, ht4, HOp(Or, cel5, ht6)))] @ ld))
  in
  let find_cstr_sl_final_noexist hcl cel5 lc ld lval ht4 ht6 ht1_cel2_ht3 =
    let c_neighbor = (search_neighbor cel5) in
    let comp = get_comp cel5 in
    let exist_c = exist_cstr (get_hterm cel5) comp hcl in 
    let exist_neighbor = eval_each_neighbor c_neighbor hcl in
    let exist_neighbor_in_lval = eval_each_neighbor_in_lval c_neighbor lval in
    let exist_c_in_lval = find_cel_in_lval lval (eval_cel cel5) comp in
    if 
      (exist_c = "exist") || 
      (exist_neighbor = "exist") || 
      (exist_c_in_lval = "exist") || 
      (exist_neighbor_in_lval = "exist")
    then (lc, [HOp(Or, ht1_cel2_ht3, ht4)] @ ld)
    else (
      if (
        (exist_c = "opposite") || 
        (exist_neighbor = "opposite") || 
        (exist_c_in_lval = "opposite") || 
        (exist_neighbor_in_lval = "opposite"))
    then (lc, [HOp(Or, ht1_cel2_ht3, HOp(And, ht4, ht6))] @ ld)
    else (lc, [HOp(Or, ht1_cel2_ht3, HOp(And, ht4, HOp(Or, cel5, ht6)))] @ ld))
  in
  (* 
    Constraint of the form
    [π'_v^i= 0 ∧ (C_v,w,k ≥ 0. ∨ π_v^i<π'_v^i − C_v,w,k × Δt] ∨ 
    [π'_v^i= 1 ∧ (C_v,w,k ≤ 0. ∨ π_v^i>π'_v^i − C_v,w,k × Δt]
    = [ht1 ∧ (cel2 ∨ ht3] ∨ [ht4 ∧ (cel5 ∨ ht6]
    with
  ht1  = π'_v^i= 0
  cel2 = C_v,w,k ≥ 0.
  ht3  = π_v^i<π'_v^i − C_v,w,k × Δt
  ht4  = π'_v^i= 1
  cel5  = C_v,w,k ≤ 0.
  ht6  = π_v^i>π'_v^i − C_v,w,k × Δt
  *)
  let find_cstr_sl hcl cel2 lc ld lval ht1 ht3 ht4 cel5 ht6 =
    let c_neighbor = (search_neighbor cel2) in
    let comp = get_comp cel2 in
    let exist_c = exist_cstr (get_hterm cel2) comp hcl in 
    let exist_neighbor = eval_each_neighbor c_neighbor hcl in
    let exist_neighbor_in_lval = eval_each_neighbor_in_lval c_neighbor lval in
    let exist_c_in_lval = find_cel_in_lval lval (eval_cel cel2) comp in
    if 
      (exist_c = "exist") || 
      (exist_neighbor = "exist") || 
      (exist_c_in_lval = "exist") || 
      (exist_neighbor_in_lval = "exist")
    then (find_cstr_sl_final_exist hcl cel5 lc ld lval ht4 ht6 ht1)
    else if 
      (exist_c = "opposite") || 
      (exist_neighbor = "opposite") || 
      (exist_c_in_lval = "opposite") || 
      (exist_neighbor_in_lval = "opposite")
    then (find_cstr_sl_final_opp hcl cel5 lc ld lval ht4 ht6 (HOp(And, ht1, ht3)))
    else (find_cstr_sl_final_noexist hcl cel5 lc ld lval ht4 ht6 (HOp(And, ht1, HOp(Or, cel2, ht3))))
  in
  let find_cstr_simpl_sl hcl cel3 lc ld lval ha1 ha2 ha4 =
    let c_neighbor = (search_neighbor cel3) in
    let comp = get_comp cel3 in
    let exist_c = exist_cstr (get_hterm cel3) comp hcl in 
    let exist_neighbor = eval_each_neighbor c_neighbor hcl in
    let exist_neighbor_in_lval = eval_each_neighbor_in_lval c_neighbor lval in
    let exist_c_in_lval = find_cel_in_lval lval (eval_cel cel3) comp in
    if 
      (exist_c = "exist") || 
      (exist_neighbor = "exist") || 
      (exist_c_in_lval = "exist") || 
      (exist_neighbor_in_lval = "exist")
    then (lc, [HOp(Or, ha2, ha1)] @ ld)
    else if 
      (exist_c = "opposite") || 
      (exist_neighbor = "opposite") || 
      (exist_c_in_lval = "opposite") || 
      (exist_neighbor_in_lval = "opposite")
    then (lc, [HOp(Or, ha1, HOp(And, ha2, ha4))] @ ld)
    else (lc, [HOp(Or, ha1, HOp(And, ha2, HOp(Or, cel3, ha4)))] @ ld)
  in
  let find_cstr_simplified_sl hcl cel2 lc ld lval ha1 ha3 ha4 =
    let c_neighbor = (search_neighbor cel2) in
    let comp = get_comp cel2 in
    let exist_c = exist_cstr (get_hterm cel2) comp hcl in 
    let exist_neighbor = eval_each_neighbor c_neighbor hcl in
    let exist_neighbor_in_lval = eval_each_neighbor_in_lval c_neighbor lval in
    let exist_c_in_lval = find_cel_in_lval lval (eval_cel cel2) comp in
    if 
      (exist_c = "exist") || 
      (exist_neighbor = "exist") || 
      (exist_c_in_lval = "exist") || 
      (exist_neighbor_in_lval = "exist")
    then (lc, [HOp(Or, ha1, ha4)] @ ld)
    else if 
      (exist_c = "opposite") || 
      (exist_neighbor = "opposite") || 
      (exist_c_in_lval = "opposite") || 
      (exist_neighbor_in_lval = "opposite")
    then (lc, [HOp(Or, HOp(And, ha1, ha3), ha4)] @ ld)
    else (lc, [HOp(Or, HOp(And, ha1, HOp(Or, cel2, ha3)), ha4)] @ ld)
  in
  let identify_cstr hcl h lc ld lval =
    match h with
      (* Constraint coming from slide(v) without knowing the sign *)
      (* [π'_v^i= 1 ∧ (C_v,w,k ≤ 0. ∨ π_v^i>π'_v^i − C_v,w,k × Δt)] ∨ [π'_v^i= 0 ∧ (C_v,w,k ≥ 0. ∨ π_v^i<π'_v^i − C_v,w,k × Δt] *)
      | HOp(
          Or,
          HOp(
            And,
            HAtom(Eq, PiExit(v1,i1), HConstant(fl1)),
            HOp(
              Or,
              HAtom(comp2, Celerity(v2,w2,k2), HConstant(fl2)),
              ht3)),
          HOp(
            And,
            HAtom(Eq, PiExit(v4,i4), HConstant(fl4)),
            HOp(
              Or,
              HAtom(comp5, Celerity(v5,w5,k5), HConstant(fl5)),
              ht6))) -> 
      find_cstr_sl hcl (HAtom(comp2, Celerity(v2,w2,k2), HConstant(fl2))) 
      lc ld lval 
      (HAtom(Eq, PiExit(v1,i1), HConstant(fl1))) 
      ht3
      (HAtom(Eq, PiExit(v4,i4), HConstant(fl4)))
      (HAtom(comp5, Celerity(v5,w5,k5), HConstant(fl5)))
      ht6
      (* [π'_v^i= 1 ∧ (C_v,w,k ≤ 0. ∨ π_v^i>π'_v^i − C_v,w,k × Δt)] ∨ [π'_v^i= 0]
      OR [π'_v^i= 1 ∧ (C_v,w,k ≤ 0. ∨ π_v^i>π'_v^i − C_v,w,k × Δt)] ∨ [π'_v^i= 0 ∧ π_v^i<π'_v^i − C_v,w,k × Δt] *)
      | HOp(
          Or,
          HOp(
            And,
            HAtom(Eq, PiExit(v1,i1), HConstant(fl1)),
            HOp(
              Or,
              HAtom(comp2, Celerity(v2,w2,k2), HConstant(fl2)),
              ha3)),
          ha4) -> 
      find_cstr_simplified_sl hcl (HAtom(comp2, Celerity(v2,w2,k2), HConstant(fl2))) 
      lc ld lval (HAtom(Eq, PiExit(v1,i1), HConstant(fl1))) ha3 ha4
      (* [π'_v^i= 1 ∧ π_v^i>π'_v^i − C_v,w,k × Δt] ∨ [π'_v^i= 0 ∧ (C_v,w,k ≥ 0. ∨ π_v^i<π'_v^i − C_v,w,k × Δt]
      OR [π'_v^i= 1] ∨ [π'_v^i= 0 ∧ (C_v,w,k ≥ 0. ∨ π_v^i<π'_v^i − C_v,w,k × Δt] *)
      | HOp(
          Or,
          ha1,
          HOp(
            And,
            HAtom(Eq, PiExit(v2,i2), HConstant(fl2)),
            HOp(
              Or,
              HAtom(comp3, Celerity(v3,w3,k3), HConstant(fl3)),
              ha4))) -> 
      find_cstr_simpl_sl hcl (HAtom(comp3, Celerity(v3,w3,k3), HConstant(fl3))) 
      lc ld lval ha1 (HAtom(Eq, PiExit(v2,i2), HConstant(fl2))) ha4
      (* ((C_v,w,k ≤ 0.) ∨ (π_v^i=π'_v^i − C_v,w,k × Δt)) ∨ ((C_v,w,k ≤ 0.) ∧ (C_v,w',k' < 0.)) *)
      | HOp(
          Or, 
          HOp(
            Or, 
            HAtom(comp1, Celerity(v1,w1,k1), HConstant(fl1)), 
            ha2), 
          HOp(
            Or, 
            HAtom(comp2, Celerity(v2,w2,k2), HConstant(fl2)), 
            HAtom(comp3, Celerity(v3,w3,k3), HConstant(fl3)))) -> 
      find_cstr_three_cel hcl (HAtom(comp1,Celerity(v1,w1,k1), HConstant(fl1)))  lc ld lval ha2 (HAtom(comp2,Celerity(v2,w2,k2), HConstant(fl2))) (HAtom(comp3,Celerity(v3,w3,k3), HConstant(fl3)))
      (* ((π'_v^i comp fl) ∨ ((C_v,w,k comp2 0.) ∧ (π_v^i=π'_v^i − C_v,w,k × Δt))) *)
      | HOp(
          Or, 
          ha1, 
          HOp(
            And, 
            HAtom(comp2, Celerity(v2,w2,k2), HConstant(fl2)), 
          ha2)) -> 
      find_cstr_one_cel hcl 
      (HAtom(comp2, Celerity(v2,w2,k2), HConstant(fl2))) 
      lc ld lval 
      (HOp(Or, ha1, ha2)) ha1 
      (HOp(Or, ha1, HOp(And, HAtom(comp2, Celerity(v2,w2,k2), HConstant(fl2)),ha2)))
      (* ((C_v,w,k comp 0.) ∨ (π_v^i=π'_v^i − C_v,w,k × Δt)) ∨ (C_v,w,k > 0) *)
      | HOp(
          Or, 
          HOp(
            Or, 
            HAtom(comp1,Celerity(v1,w1,k1), HConstant(fl1)), 
            ha2), 
          HAtom(comp2,Celerity(v2,w2,k2), HConstant(fl2))) -> 
      find_cstr_two_cel hcl (HAtom(comp1,Celerity(v1,w1,k1), HConstant(fl1))) lc ld lval ha2 (HAtom(comp2,Celerity(v2,w2,k2), HConstant(fl2))) (HOp(Or, HAtom(comp2,Celerity(v2,w2,k2), HConstant(fl2)), ha2)) h
      (* We analyse (C_v,w,k comp 0. ∨ C_v,w',k' < 0.) et 
      (C_v,w,k comp 0. ∨ π_v^i≤π'_v^i − C_v,w,k × Δt) *)
      | HOp(
          Or, 
          HAtom(comp1, Celerity(v1,w1,k1), HConstant(fl1)), 
          ha2) -> 
      find_cstr hcl h lc ld lval
      | other -> (lc, [other] @ ld)
    in
  (** Reversed list of HybridDisjunction for fixpoint computation **)
  let rec reversed_list l lf =
    match l with
      | [] -> lf
      | h::[] -> [h] @ lf
      | h::t -> reversed_list t ([h] @ lf)
    in
  let rec hd_list hcl hdl lc ld lval = (* Assumes all element in the list *)
    match hdl with
      | [] -> raise(Invalid_argument "hd_list: Error")
      | h :: [] -> 
        let res = identify_cstr hcl h lc ld lval in
        ((fst res) @ hcl, (reversed_list (snd res) []))
      | h :: t -> 
        let res = identify_cstr hcl h lc ld lval in
        hd_list hcl t (fst res) (snd res) lval
    in
  let get_tuple s =
    match s with
      | (lval, cstr) -> (lval, (hd_list (fst cstr) (snd cstr) [] [] lval))
    in
  let rec eval_all_element s l =
    match s with
      | [] -> raise(Invalid_argument "eval_all_element: list sval empty")
      | h :: [] -> [get_tuple h] @ l
      | h :: t -> eval_all_element t ([get_tuple h] @ l)
    in eval_all_element sval []
;;


(* The function below determines if some constraints of touch delays (included
in "disjunctive formula") exist. We search the current touch delay in the 
hybridConjunction list. If it doesn't exist, we let it in the "disjunctive 
formula" list *)
let simplify_cstr_touch_delay sval =
  let exist_cstr_touch_delay c1 c2 =
    if (((c1 = GT)  && (c2 = GT)) ||
        ((c1 = LT)  && (c2 = LT)) ||
        ((c1 = GE)  && (c2 = GT)) ||
        ((c1 = GE)  && (c2 = GE)) ||
        ((c1 = GE)  && (c2 = Eq)) ||
        ((c1 = LE)  && (c2 = LT)) ||
        ((c1 = LE)  && (c2 = LE)) ||
        ((c1 = LE)  && (c2 = Eq)) ||
        ((c1 = Eq)  && (c2 = Eq)) ||
        ((c1 = NEq) && (c2 = GT)) ||
        ((c1 = NEq) && (c2 = LT)) ||
        ((c1 = NEq) && (c2 = NEq)))
    then ("exist")
    else (
      if (((c1 = GT)  && (c2 = LT)) ||
          ((c1 = GT)  && (c2 = LE)) ||
          ((c1 = GT)  && (c2 = Eq)) ||
          ((c1 = LT)  && (c2 = GT)) ||
          ((c1 = LT)  && (c2 = GE)) ||
          ((c1 = LT)  && (c2 = Eq)) ||
          ((c1 = GE)  && (c2 = LT)) ||
          ((c1 = LE)  && (c2 = GT)) ||
          ((c1 = Eq)  && (c2 = GT)) ||
          ((c1 = Eq)  && (c2 = LT)) ||
          ((c1 = Eq)  && (c2 = NEq)) ||
          ((c1 = NEq) && (c2 = Eq)))
      then ("opposite")
      else ("notexist"))
    in
  let rec get_hterms h = (* Give the hybridCondition ht1 of the formula *)
    match h with 
      | HAtom(comp, ht1, ht2) -> (ht1,ht2)
      | _ -> raise(Invalid_argument "get_hterms: Error")
    in
  let rec get_comp h = (* Give the comparator of the hybridCondition HAtom*)
    match h with
      | HAtom(comp, ht1, ht2) -> comp
      | _ -> raise(Invalid_argument "get_comp: Error")
    in
  let rec assess_formula hd h2 = 
    (* We analyse the constraints of the form : 
      C_{v,w,k<n} > 0 ∧ C_{v,w,k>n} < 0 
    AND PiEntrance(v,i) comp PiExit(v,i) - C_{v,w,n} *) 
    let comp_hd = get_comp hd in 
    match h2 with
      | HOp(op, h1, h2) -> 
        let hterms_hd = get_hterms hd in
        let hterms_h1 = get_hterms h1 in 
        if (fst hterms_hd = fst hterms_h1) && (snd hterms_hd = snd hterms_h1)
        then (exist_cstr_touch_delay (get_comp h1) comp_hd)
        else assess_formula hd h1
      | HAtom(comp, ht1, ht2) -> 
        let hterms_hd = get_hterms hd in
        if (fst hterms_hd = ht1) && (snd hterms_hd = ht2)
        then (exist_cstr_touch_delay (get_comp h2) comp_hd)
        else ("notexist")
      | _ -> raise(Invalid_argument "assess_formula: should be of the form C_{v,w,k<n} > 0 ∧ C_{v,w,k>n} < 0, or 
        of the form PiEntrance(v,i) comp PiExit(v,i) - C_{v,w,n}")
    in
  let rec find_cstr_on_touch_delay_comp hcl lc ld h1 h2 hcl_initial =
    match hcl with
      | [] -> (lc, ([(h1,h2)] @ ld))
      | h :: [] ->
        let assess_form = assess_formula h h2 in
          if (assess_form = "exist")
          then ((lc, ld))
          else (
            if (assess_form = "opposite")
            then (([h1] @ lc, ld))
            else ((lc, ([(h1,h2)] @ ld))))
      | h :: t -> 
        let assess_form = assess_formula h h2 in
        if (assess_form = "exist")
        then ((lc, ld))
        else (
          if (assess_form = "opposite")
          then (([h1] @ lc, ld))
          else (find_cstr_on_touch_delay_comp t lc ld h1 h2 hcl_initial))
    in
  let find_cstr hcl h lc ld lval = 
    match h with
      | HOp(op, h1, h2) -> find_cstr_on_touch_delay_comp hcl lc ld h1 h2 hcl
      | other -> raise(Invalid_argument "find_cstr: Error")
    in
  let rec hd_list hcl hdl lc ld lval = (* Assumes all element in the list *)
    match hdl with
      | [] -> (lc @ hcl, ld)
      | h :: [] -> 
        let res = find_cstr hcl h lc ld lval in
        ((fst res) @ hcl, snd res)
      | h :: t -> 
        let res = find_cstr hcl h lc ld lval in
        hd_list hcl t (fst res) (snd res) lval
    in
  let get_tuple s =
    match s with
      | (lval, cstr) -> (lval, (hd_list (fst cstr) (snd cstr) [] [] lval))
    in
  let rec eval_all_element s l =
    match s with
      | [] -> raise(Invalid_argument "eval_all_element: list sval empty")
      | h :: [] -> [get_tuple h] @ l
      | h :: t -> eval_all_element t ([get_tuple h] @ l)
    in eval_all_element sval []
;;

let select_hdisj s = 
  let rec select_hdisjl sval =
    match sval with
      | [] -> raise(Invalid_argument "select_hdisjl: super valuation list empty")
      | h::[] -> 
        (match h with
          | (lval, (hConj, hDisj)) -> [hDisj])
      | h::t ->
        (match h with
          | (lval, (hConj, hDisj)) -> [hDisj] @ (select_hdisjl t))
    in select_hdisjl s ;;


(***********************************)
(*** Simplify Hybrid Conjunction ***)
(***********************************)
(* get_hconj function select hybridConjunction list to display into the output files *)
let get_hconj sval =
  let rec select_hconj s =
    match s with
      | (lval, (hCond,_)) -> hCond
    in
  let rec select_element s l =
    match s with
      | [] -> raise(Invalid_argument "select_element: lvaluation empty")
      | h::[] -> [select_hconj h] @ l
      | h::t -> select_element t ([select_hconj h] @ l)
    in
    select_element sval [];;


(* We replace all HOp() by the elements of the corresponding HAtom() in order to 
make the next simplification easier  *)
let replace_HOp_in_hconl hl =
  let rec replace_HOp h =
    match h with
      | HOp(op, h1, h2) -> (replace_HOp h1) @ (replace_HOp h2)
      | other_hterm -> [other_hterm]
    in
  let rec select_each_hterm l =
    match l with
      | [] -> raise(Invalid_argument "select_each_hterm: one hconj list empty")
      | h :: [] -> replace_HOp h
      | h :: t -> replace_HOp h @ (select_each_hterm t)
    in
  let rec select_each_elt l =
    match l with
      | [] -> raise(Invalid_argument "select_each_elt: all hconj lists empty")
      | h :: [] -> [select_each_hterm h]
      | h :: t -> [select_each_hterm h] @ (select_each_elt t)
  in select_each_elt hl;;


(* We simplify the hybrid conjunction list in removing the replicate of a formula,
or reducing formulas (A >= B && A != B ...)  *)
let simplify_hconj hconj =
  let rec in_list elt l =
    match l with
      | [] -> false
      | h :: t -> if (elt = h) then (true) else (in_list elt t)
    in
  let new_comp c1 c2 ht1 ht2 =
    if (((c1 = LT)  && (c2 = LT))  ||
        ((c1 = LT)  && (c2 = LE))  ||
        ((c1 = LT)  && (c2 = NEq)) ||
        ((c1 = LE)  && (c2 = LT))  ||
        ((c1 = LE)  && (c2 = NEq)) ||
        ((c1 = NEq) && (c2 = LT))  ||
        ((c1 = NEq) && (c2 = LE)))
    then ((LT, 0.))
    else (
        if ((c1 = GT)   && (c2 = GT))  ||
            ((c1 = GT)  && (c2 = GE))  ||
            ((c1 = GT)  && (c2 = NEq)) ||
            ((c1 = GE)  && (c2 = GT))  ||
            ((c1 = GE)  && (c2 = NEq)) ||
            ((c1 = NEq) && (c2 = GT))  ||
            ((c1 = NEq) && (c2 = GE))
        then ((GT, 0.))
        else (
          if ((c1 = LE) && (c2 = LE))
          then ((LE, 0.))
          else (
            if ((c1 = GE) && (c2 = GE))
            then ((GE, 0.))
            else (
              if ((c1 = LE) && (c2 = GE)) ||
                 ((c1 = LE) && (c2 = Eq)) ||
                 ((c1 = GE) && (c2 = LE)) ||
                 ((c1 = GE) && (c2 = Eq)) ||
                 ((c1 = Eq) && (c2 = LE)) ||
                 ((c1 = Eq) && (c2 = GE)) ||
                 ((c1 = Eq) && (c2 = Eq))            
              then ((Eq, 0.))
              else (
                if ((c1 = NEq) && (c2 = NEq))
                then ((NEq, 0.))
                else (raise (Invalid_argument (
                  "Opposite formulas found.  Check your Hoare triple (the boundary of the variables, the path or the Celerities defined in the Postcondition.) :\n" ^ (string_of_hybridCondition ht1) ^ " and " ^ (string_of_hybridCondition ht2)))))))))
  in
  let rec simplify_formula ha l lf =
    match l with
      | [] -> if (in_list ha lf) then ([]) else ([ha])
      | h :: t -> 
        match (ha,h) with
          | (HAtom(comp1, ht1_1, ht1_2), HAtom(comp2, ht2_1, ht2_2)) ->
            if (ht1_1 = ht2_1 && ht1_2 = ht2_2)
            then (
              let (comp, fl) = new_comp comp1 comp2 (HAtom(comp1, ht1_1, ht1_2)) (HAtom(comp2, ht2_1, ht2_2)) in
              let new_hatom = HAtom(comp, ht1_1, ht1_2) in
              if (in_list new_hatom lf) then ([]) else ([new_hatom]) )
            else (
              if (ht1_1 = ht2_1 &&
                  (ht1_2 = HConstant(0.) && ht2_2 = HConstant(0.)))
              then (
                let (comp, fl) = new_comp comp1 comp2 (HAtom(comp1, ht1_1, ht1_2)) (HAtom(comp2, ht2_1, ht2_2)) in
                let new_hatom = HAtom(comp, ht1_1, HConstant(fl)) in
                if (in_list new_hatom lf) then ([]) else ([new_hatom]))
              else (simplify_formula ha t lf))
          | (_, _) -> simplify_formula ha t lf
    in
  let rec simplify_hconj_list l ld lf =
    match l with
      | [] -> lf 
      | h :: t ->
        let list_without_current_elt = List.filter (fun x -> x <> h) ld in
        simplify_hconj_list t ld ((simplify_formula h list_without_current_elt lf) @ lf)
    in
  let rec select_each_list hc =
    match hc with
      | [] -> raise(Invalid_argument "select_each_list: all hconj list empty")
      | h :: [] -> [simplify_hconj_list h h []]
      | h :: t -> [simplify_hconj_list h h []] @ (select_each_list t)
    in select_each_list hconj;;

