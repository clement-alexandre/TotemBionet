(*********************************************************************)
(*** Computation of the strongest postcondition as a ste of states ***)
(*********************************************************************)

open Types;;
open Extraction_data;;
open Set_states;;

(**
The aim of this part is to obtain the list of states corresponding
to the strongest postcondition of a program,
given the computation of the weakest precondition,
which itself was obtained from the same program and a given postcondition.
In other words, from a given couple (program, postcondition),
we compute the weakest precondition as a (general) discrete condition,
then the corresponding strongest postcondition as a set of states.
the final result is the initial postcondition, but as a list of states,
and rid of all states that are incompatible with an execution of the program.

The computation of the weakest precondition is made in a classical way,
with predicate calculus (that is, modifications on the condition)
and the backward strategy (the program is traversed backwards).
this weakest precondition is then translated into an equivalent set of states
and the strongest postcondition is obtained with the forward strategy
(traversing the program forwards) by eliminating impossible states
(states that overflow on one of the variables).

The final set of states is intended to be re-injected while computing
the hybrid part of the weakest precondition.
**)

(*
L'objectif de cette partie est d'obtenir la liste des états correspondant
à la plus forte postcondition d'un programme,
en fonction du calcul de la plus faible précondition,
lui-même obtenu à partir du programme et d'une postcondition donnée.
En d'autres termes, à partir d'un couple de départ (programme, postcondition),
calculer la plus faible précondition discrète de ce couple sous forme de condition
puis la plus forte postcondition sous forme de liste d'états.
Le résultat final est la postcondition de départ, sous la forme d'une liste d'états,
et débarrassée de tous les états incompatibles avec le programme.

Le calcul de la plus faible précondition est réalisé de façon classique,
par calcul de prédicats (c.-à-d. par modification de la condition)
en parcourant le programme en arrière (backward strategy).
Cette plus faible précondition est ensuite traduite en liste d'états
équivalente, et la plus forte postcondition est obtenue en parcourant
le programme en avant (forward strategy) et en éliminant les états
impossibles (par dépassement de borne d'une des variables).

La liste des états finaux a pour vocation d'être réinjectée lors
du calcul de la partie hybride de la plus faible précondition.
*)

(** Replace expression e1 by expression e2 in dc **)
let replaceDiscreteTerm (dc:discreteCondition) (e1:discreteTerm) (e2:discreteTerm) =
  let rec replaceTerm (dt:discreteTerm) =   (* Recursion on a discrete term *)
    if dt = e1 then e2 else   (* If sought term: replace *)
      match dt with   (* Else: carry on recursion *)
        | DTermRelation(s, dt1, dt2) -> DTermRelation(s, replaceTerm dt1, replaceTerm dt2)
        | _ -> dt in    (* Eta or DConstant *)
  let rec replaceCondition (dc:discreteCondition) =   (* Recursion in a discrete condition *)
    match dc with
      | DBoolean(_) -> dc
      | DNeg(n, dc1) -> (match n with | Neg -> DNeg(Neg, replaceCondition dc1))
      | DAtom(c, dt1, dt2) -> DAtom(c, replaceTerm dt1, replaceTerm dt2)
      | DOp(op, dc1, dc2) -> DOp(op, replaceCondition dc1, replaceCondition dc2) in
  replaceCondition dc ;;

(** Compute the weakest precondition of the discrete condition **)
let rec discreteWP (prog:pathLanguage) (post:discreteCondition) =
  let action (v, s:var * symbol) =    (* Substitution for the instruction v+/v− *)
    match s with
      | PlusSymbol -> DTermRelation(DPlus, (Eta v), (DConstant 1))
      | MinusSymbol -> DTermRelation(DMinus, (Eta v), (DConstant 1)) in
  match prog with   (* Recursive definition of the WP *)
    | Nothing -> post
    | Tuple(time, a, instr) -> replaceDiscreteTerm post (Eta(varDpa instr)) (action instr)
    | Seq(p1, p2) -> discreteWP p1 (discreteWP p2 post);;

(** Compute a neighbouring state with an option **)
let option_neighbour_state (s:state) (v:var) (pm:symbol) =
  let eval_symbol = function
    | PlusSymbol -> (+)
    | MinusSymbol -> (-) in
  let option_cons h = function
    | None -> None
    | Some l -> Some (h :: l) in
  let rec rec_neighbour_state (cs:state) (cvarlist:var list) =
    match cs, cvarlist with
      | hcs :: tcs, hcv :: tcv ->
        (if varequals v hcv
          then let newhcs = (eval_symbol pm) hcs 1 in
            (if newhcs >= 0 && newhcs <= (getbound v)
              then Some (newhcs :: tcs)
              else None)
          else option_cons hcs (rec_neighbour_state tcs tcv))
      | _, _ -> raise Not_found in
  rec_neighbour_state s varlist ;;

(** Backward (WP) then forward (SP) computation of the compatible states **)
let discreteWPSP (prog:pathLanguage) (post:discreteCondition) =
      (* The states compatible with the weakest precondition (without borders) *)
  let backward = get_ze_states (discreteWP prog post) in
      (* Clean up the None's and keep only the values from an option list *)
  let rec list_filter_option = function
    | [] -> []
    | h :: t -> match h with
                  | None -> list_filter_option t
                  | Some e -> e :: (list_filter_option t) in
      (* Forward computation of the compatible states (with borders) *)
  let rec forward (ls:state list) (instr:pathLanguage) =
    match instr with
      | Nothing -> ls
      | Tuple(t, a, (v, pm)) ->
          list_filter_option (List.map (fun s -> option_neighbour_state s v pm) ls)
      | Seq(instr1, instr2) -> forward (forward ls instr1) instr2 in
  forward backward prog ;;