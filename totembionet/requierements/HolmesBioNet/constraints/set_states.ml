(***********************************************************)
(*** Extraction of the set of states equivalent to a DNF ***)
(***********************************************************)

open Types;;
open Display;;
open Extraction_data;;
open Dnf_discrete_condition;;

(**
The aim of this part is to create the list (set) of states
that a discrete condition characterizes.

The first step is to rewrite the information of a DNF (see previous part)
into a list of pseudo-states (type pseudostate).
A pseudo-state is an association list, that is, a list of couples (v, i)
where v is a variable and i an integer constant,
in which each variable appears only once.
A variable may not appear inside a pseudo-state; the information is thus
incomplete and several states may correspond to this pseudo-state.
In other words, we here switch from a tree representation (discrete condition)
to a list representation.
Incoherent conjunctions (containing two contradicting atoms) are discarded.

In the end, we are able to build a list of states for each pseudo-state.
A state (type state) is a list of integers in which each element corresponds
to the expression level of a variable (in the same order as in the list vars).
the second step thus consists in creating the entire set of states compatible
with a given pseudo-state by enumerating all possible values of the variables
that are represented by this pseudo-state.
The list of states obtained from all pseudo-states then matches exactly
all the states described by the initial discrete condition.
The list is sorted and duplicates are removed.
**)

(*
Cette partie a pour but de créer la liste des états
représentés par une condition discrète.

La première étape a pour but de réécrire les informations d'une DNF unitaire
(voir partie précédente) sous la forme d'une liste de pseudo-états (pseudostate).
Un pseudo-état est une liste d'associations, c.-à-d. une liste de couples (v, i)
avec v une variable et i une constante entière,
dans laquelle chaque variable n'apparaît qu'une seule fois.
Une variable peut ne pas figurer dans un pseudo-état ; l'information est alors
partielle et plusieurs états peuvent correspondre à ce pseudo-état.
Il s'agit en somme de passer d'une représentation arborescente (condition discrète)
à une représentation sous forme de liste.
Les conjonctions incohérentes (contenant deux atomes en contradiction) sont écartées.

On est enfin en mesure de créer une liste d'états correspondant à chaque
pseudo-état. Un état (state) est une liste d'entiers dont chaque élément
représente le niveau d'expression d'une variable (dans le même ordre que vars).
La deuxième étape consiste donc à créer tous les états compatibles avec
un pseudo-état donné en énumérant toutes les valeurs possibles
des variables non représentées dans ce pseudo-état.
La liste d'états obtenue à partir de tous les pseudo-états
recouvre exactement tous les états décrits par la condition discrète d'origine.
La liste est triée et les doublons sont supprimés.
*)

(** State (integer list) and pseudostate (var * int association list) **)
type state = int list ;;
type pseudostate = (var * int) list ;;

(** Translation of a state or pseudostate into a string **)
let string_of_state = string_of_list_delim string_of_int ";" "<" ">" ;;

(** Translate a DNF into association lists **)
let list_of_unitDNF (c:discreteUnitCondition) =
      (* Take into account the atom in the state (add or invalidate) *)
  let list_of_unitDNF_atom (v:var) (i:int) (lo:pseudostate option) =
    match lo with
      | None -> None
      | Some l -> if List.mem_assoc v l
                        (* If a ref already exists, check compatibility *)
                    then if List.assoc v l = i then lo else None
                        (* Else, add it *)
                    else Some ((v, i) :: l) in
      (* Recursion on an inner conjunction *)
  let rec list_of_unitDNF_conj (cc:discreteUnitCondition) (lo:pseudostate option) =
    match cc with
      | DUnitBoolean(d) -> (match d with
          | True -> lo
          | False -> None)
      | DUnitOp(op, cc1, cc2) -> (match op with
          | PosOr -> raise (Invalid_argument "list_of_unitDNF.list_of_unitDNF_conj: not a DNF")
          | PosAnd -> list_of_unitDNF_conj cc1 (list_of_unitDNF_conj cc2 lo))
      | DUnitAtomEq(v, i) -> list_of_unitDNF_atom v i lo in
  let call_list_of_unitDNF_conj (cc:discreteUnitCondition) =
    match list_of_unitDNF_conj cc (Some []) with
      | Some l -> [l]
      | None -> [] in
      (* Recursion on the outer disjunction *)
  let rec list_of_unitDNF_disj (cd:discreteUnitCondition) =
    match cd with
      | DUnitBoolean(d) -> (match d with
          | True -> [[]]
          | False -> [])
      | DUnitOp(op, c1, c2) -> (match op with
              (* Enter a conjunction*)
          | PosAnd -> call_list_of_unitDNF_conj cd
              (* Still in the disjunction *)
          | PosOr -> (list_of_unitDNF_disj c1) @ (list_of_unitDNF_disj c2))
      | DUnitAtomEq(_, _) -> call_list_of_unitDNF_conj cd in
  list_of_unitDNF_disj c ;;

(** Enumerate the states compatible with a list of pseudo-states **)
let rec states_of_pseudostates (l:pseudostate list) =
      (* enumerate integers from 0 to n *)
  let rec rec_enum_n n l = if n <= 0 then 0 :: l else rec_enum_n (n - 1) (n :: l) in
  let enum_n n = List.rev (rec_enum_n n []) in
      (* Revert order of the variable list *)
  let revvarlist = List.rev varlist in
      (* Prefix each state (list) with i *)
  let prepend_all (ls:state list) (i:int) =
    List.map (fun s -> i :: s) ls in
      (* Enumerate the states compatible with a given pseudo-state *)
  let rec enum_states (ps:pseudostate) (vl:var list) (ls:state list) =
    match vl with
      | [] -> ls
      | v :: t -> if List.mem_assoc v ps
                        (* If v is in this pseudo-state, add only the corresponding value *)
                    then enum_states ps t (prepend_all ls (List.assoc v ps))
                        (* Else, add all possible values *)
                    else enum_states ps t
                      (List.fold_left
                        (fun ls2 i -> (prepend_all ls i) @ ls2)
                        [] (enum_n (getbound v))) in
      (* Browse all pseudo-states *)
  let rec rec_states_of_pseudostates (lps:pseudostate list) =
    match lps with
      | [] -> []
      | ps :: t -> (enum_states ps revvarlist [[]]) @ (rec_states_of_pseudostates t) in
  (List.sort_uniq compare (rec_states_of_pseudostates l)) ;;

(** Final result of this part:
    Return the set of states (represented as lists)
    compatible with a given general discrete condition **)
let get_ze_states (dc:discreteCondition) = states_of_pseudostates (list_of_unitDNF (dnf dc)) ;;