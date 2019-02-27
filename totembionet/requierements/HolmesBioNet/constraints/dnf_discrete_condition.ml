(**************************************************************)
(*** Computation of the unitary DNF of a discrete condition ***)
(**************************************************************)

open Types;;
open Display;;
open Extraction_data;;
open Standard_functions;;

(**
This part aims at translating a general discrete condition
(usually obtained by weakest precondition calculus) into
a unitary DNF (Disjunctive Normal Form), that is,
containing only disjunctions of conjunctions, and in which
every atom is of the form: η_x = const
(only equalities with the discrete state of a variable x on the left
and a term that simplifies into a constant on the right).

The point is to extract a set of states compatible with each term
of the disjunction and re-inject these states in the hybrid condition
in order to simplify it (somehow on-the-fly).
**)

(*
Cette partie a pour but de transformer une condition discrète générique
(habituellement obtenue par plus faible précondition) en DNF unitaire,
c'est-à-dire ne contenant que des disjonctions de conjonctions,
et dont chaque atome s'écrit de la forme suivante : η_x = const
(uniquement des égalités avec à gauche la partie discrète d'une variable x
et à droite un terme qui se simplifie en une constante).

L'objectif est d'extraire un ensemble d'états de chaque terme de la disjonction
et de réinjecter ces états dans la condition hybride pour la simplifier
(si possible à la volée).
*)

(**
First, the (general) discrete condition is translated into
a positive condition, that is, without negations.
This is achieved with De Morgan laws
or by reversing the comparator inside the atoms.
**)

(*
Dans un premier temps, la condition discrète (générique) est transformée
en condition positive, c'est-à-dire sans négation.
Cela est obtenu par application des lois de De Morgan
ou en inversant le comparateur dans les atomes.
*)

(** Positive connectives: we also remove implications for simplicity **)
type positiveOperator =
  | PosAnd | PosOr ;;

(** Discrete positive condition: no negations **)
type discretePositiveCondition =
  | DPosBoolean of boolean
  | DPosOp of positiveOperator * discretePositiveCondition * discretePositiveCondition
  | DPosAtom of comp * discreteTerm * discreteTerm ;;


(** Translate a (general) discrete condition into a positive condtiion **)
let discretePositiveCondition_of_discreteCondition (f:discreteCondition) =
  let neg_comp = function   (* Negation of a comparator *)
    | Eq -> NEq
    | NEq -> Eq
    | LE -> GT
    | LT -> GE
    | GE -> LT
    | GT -> LE in
  let neg_boolean = function    (* Negation of a Boolean *)
    | True -> False
    | False -> True in
  let rec dPC_of_dC_even = function   (* Remove negations (positive/even case) *)
    | DBoolean(b) -> DPosBoolean(b)
    | DNeg(n, dc) -> (match n with | Neg -> dPC_of_dC_odd dc)
    | DOp(op, dc1, dc2) -> (match op with
        | And -> DPosOp(PosAnd, dPC_of_dC_even dc1, dPC_of_dC_even dc2)
        | Or -> DPosOp(PosOr, dPC_of_dC_even dc1, dPC_of_dC_even dc2)
        | Impl -> DPosOp(PosOr, dPC_of_dC_even dc2, dPC_of_dC_even (DNeg(Neg, dc1))))
    | DAtom(comp, dt1, dt2) -> DPosAtom(comp, dt1, dt2)
  and dPC_of_dC_odd = function    (* Remove negations (negative/odd case) *)
    | DBoolean b -> DPosBoolean (neg_boolean b)
    | DNeg(n, dc) -> (match n with | Neg -> dPC_of_dC_even dc)
    | DOp(op, dc1, dc2) -> (match op with
        | And -> DPosOp(PosOr, dPC_of_dC_odd dc1, dPC_of_dC_odd dc2)
        | Or -> DPosOp(PosAnd, dPC_of_dC_odd dc1, dPC_of_dC_odd dc2)
        | Impl -> dPC_of_dC_odd (DOp(Or, dc2, DNeg(Neg, dc1))))
    | DAtom(comp, dt1, dt2) -> DPosAtom(neg_comp comp, dt1, dt2)
in dPC_of_dC_even f ;;


(**
Then, the (discrete) positive condition is reduced, that is,
each atom rewritten under the form η_x [comp] const
where η_x is the discrete state of a variable x,
[comp] is one of the usual comparators (=, ≠, <, ≤, >, ≥)
and const is an integer constant.
For this, we use basic arithmetic rules allowing to preserve
the equivalence between the original and final atoms.

At this step, the number of variables (in a general meaning, that is,
term that are not redictible to an explicit constant, such as η_x)
is counted at each side of the atom.
Currently, the program can only tackle cases that are
0/0 (constants on each side),
1/0 and 0/1 (exactly one variable, in only one side).
The other cases raise an exception as they cannot be tackled easily.
This program thus has two prerequisites about atoms:
- a discrete state η_x can only appear once
    (which is easily obtained with a manual simplification),
- two different discrete parts η_x and η_y are never compared
    (which, biologically speaking, is rather meaningless anyway).
**)

(*
Ensuite, la condition (discrète) positive est réduite,
c'est-à-dire réécrite sous la forme η_x [comp] const
où η_x est la partie fractionnaire d'une variable x,
[comp] est l'un des comparateurs (=, ≠, <, ≤, >, ≥)
et const est une constante entière.
Cela est réalisé à partir de règles arithmétiques basiques
permettant de conserver l'équivalence entre l'atome de départ et d'arrivée.

À cette étape, le nombre de variables (au sens large, c'est-à-dire des termes
non réductibles à une constante explicite et connue, par exemple η_x)
est compté de chaque côté de l'atome.
Le programme ne peut traiter que les cas 0/0 (des constantes de chaque côté),
1/0 et 0/1 (une seule variable, d'un seul côté).
Les autres cas retournent un exception car ils ne peuvent pas être traités simplement.
Le programme repose donc sur deux prérequis :
- une partie discrète η_x n'apparaît qu'une seule fois
    (ce qui peut facilement être obtenu par simplification),
- deux parties discrètes η_x et η_y ne seront jamais comparées
    (ce qui, biologiquement, n'a de toutes façons pas de sens).
*)

(** Positive reduced condition (all atoms are reduced) **)
type discreteReducedCondition =
  | DRedBoolean of boolean
  | DRedOp of positiveOperator * discreteReducedCondition * discreteReducedCondition
  | DRedAtomVar of comp * var * int ;;


(** Score of a discrete term (count of the number of variables:
    = 0 if contains only constants; > 0 if contains variables) **)
let rec score_discreteTerm = function
  | Eta(v) -> 1
  | DConstant(i) -> 0
  | DTermRelation(s, dt1, dt2) -> (score_discreteTerm dt1) + (score_discreteTerm dt2) ;;

(** Evaluation of a discrete term **)
let rec eval_discreteTerm = function
  | DConstant(i) -> i
  | Eta(v) -> raise (Invalid_argument "eval_discreteTerm: variables cannot be evaluated")
  | DTermRelation(s, dt1, dt2) ->
      eval_discreteTermSymbol s (eval_discreteTerm dt1) (eval_discreteTerm dt2) ;;


(** Reduction of an atom DAtom(comp, Eta(v), e) where e can be simplified into a constant **)
let rec reduce_discreteAtom (comp:comp) (dt1:discreteTerm) (dt2:discreteTerm) =
  (* Return the term with a variable on the left and the constant one on the right *)
  let lvar_rconst dt1 dt2 =
    if score_discreteTerm dt1 = 1
      then dt1, dt2
      else dt2, dt1 in
  (* Recursive reduction *)
  let rec reduce_terms tv tc =
    match tv with
      | Eta(v) -> DRedAtomVar(comp, v, eval_discreteTerm tc)
      | DConstant(i) -> raise (Invalid_argument "reduce_discreteAtom: reduce_terms")
      | DTermRelation(s, dtt1, dtt2) ->
        let (ttv, ttc) = lvar_rconst dtt1 dtt2 in
          match s with
            | DPlus -> reduce_terms ttv (DTermRelation(DMinus, tc, ttc))
            | DMinus ->
              if ttv = dtt1
                then reduce_terms ttv (DTermRelation(DPlus, tc, ttc))
                else reduce_terms (DTermRelation(DPlus, ttv, tc)) ttc in
  let call_reduce_terms dt1 dt2 =
    let (tv, tc) = lvar_rconst dt1 dt2 in
      reduce_terms tv tc in
      (* If several non-reducible terms: raise exception *)
  if (score_discreteTerm dt1) + (score_discreteTerm dt2) > 1
    then raise (Not_implemented ("reduce_discreteAtom: cannot reduce " ^
                                 "when several variables are involved"))
        (* If no non-reducible term: return as is *)
    else if (score_discreteTerm dt1) + (score_discreteTerm dt2) = 0
      then DRedBoolean(boolean_of_bool
             ((eval_comp comp) (eval_discreteTerm dt1) (eval_discreteTerm dt2)))
      else call_reduce_terms dt1 dt2 ;;

(** Recursive reduction of all atoms in a discrete positive condition **)
let rec reduce_discretePositiveCondition = function
  | DPosBoolean(b) -> DRedBoolean(b)
  | DPosOp(op, dc1, dc2) ->
      DRedOp(op, reduce_discretePositiveCondition dc1, reduce_discretePositiveCondition dc2)
  | DPosAtom(comp, dt1, dt2) -> reduce_discreteAtom comp dt1 dt2 ;;


(**
The reduced condition obtained must then be unitarized,
that is, must only contain atoms of the form η_x = const
(that is, using only the equality comparator).
This is obtained by fractioning each atom with any operator
into an equivalent disjunction of atoms using the comparator “=”.
For this, we simply enumerate the discrete expression levels of variable x
that meet the initial atom and build as many unitary atoms.

In the case where no expression level is compatible with the atom,
the atom is replaced by FALSE.
In the case where all possible expression levels are compatible with the atom,
the atom is replaced by TRUE (for simplification).
**)

(*
La condition réduite obtenue doit ensuite être rendue unitaire,
c'est-à-dire ne contenant que des atomes de la forme η_x = const
(en d'autres termes, en n'utilisant que le comparateur d'égalité).
Cela est obtenu en fractionnant un atome avec un comparateur quelconque
en une disjonction équivalente d'atomes avec le comparateur « = ».
Il suffit pour cela d'énumérer les niveaux discrets de la variable x
qui respectent la contrainte d'origine et de construire autant
d'atomes unitaires.

Dans le cas où aucun niveau ne vérifie la contrainte,
l'atome est remplacé par FAUX.
Dans le cas où tous les niveaux vérifient la contrainte,
l'atome est remplacé par VRAI (par mesure de simplification).
*)

(** Unitary positive condition:
    no negations and equality atoms (Eta(v) = const) only **)
type discreteUnitCondition =
  | DUnitBoolean of boolean
  | DUnitOp of positiveOperator * discreteUnitCondition * discreteUnitCondition
  | DUnitAtomEq of var * int ;;

(** Translation of a unitary positive condition into a string **)
(* let rec discreteCondition_of_discreteUnitCondition = function
  | DUnitBoolean(b) -> DBoolean(b)
  | DUnitOp(posop, duc1, duc2) -> DOp(operator_of_positiveOperator posop,
      discreteCondition_of_discreteUnitCondition duc1,
      discreteCondition_of_discreteUnitCondition duc2)
  | DUnitAtomEq(v, i) -> DAtom(Eq, Eta(v), DConstant(i)) ;;

let string_of_discreteUnitCondition dc =
  string_of_discreteCondition (discreteCondition_of_discreteUnitCondition dc) ;;
 *)
(** Translate an atom into a unitary positive disjunction (equalities only) **)
let unitarize_discreteAtom (comp:comp) (v:var) (i:int) =
      (* Enumerate all integers from 0 to n *)
  let rec rec_enum_n n l = if n <= 0 then 0 :: l else rec_enum_n (n - 1) (n :: l) in
  let enum_n n = rec_enum_n n [] in
      (* Shape of an atom *)
  let this_atom n = DUnitAtomEq(v, n) in
      (* Enumerate the set of unitary atoms equivalent to the given atom *)
  let rec enum_atoms l =
    match l with
      | [] -> DUnitBoolean(False)
      | h :: [] -> this_atom h
      | h :: t -> DUnitOp(PosOr, this_atom h, enum_atoms t) in
      (* List of levels compatible with the atom and that are enumerated *)
  let lf = List.filter (fun n -> (eval_comp comp) n i) (enum_n (getbound v)) in
      (* Special case if the list contains all levels of v *)
  if lf = (enum_n (getbound v))
    then DUnitBoolean(True)   (* Then simplify into True *)
    else enum_atoms lf ;;   (* Else enumerate all compatible atoms *)

(** Translate a condition into a unitary positive condition (equalities only) **)
let unitarize_discreteCondition (dc:discreteCondition) =
  let rec unitarize_discreteReducedCondition (rdc:discreteReducedCondition) =
    match rdc with
      | DRedBoolean(b) -> DUnitBoolean(b)
      | DRedOp(op, drc1, drc2) -> DUnitOp(op,
          unitarize_discreteReducedCondition drc1, unitarize_discreteReducedCondition drc2)
      | DRedAtomVar(comp, v, i) -> unitarize_discreteAtom comp v i in
  unitarize_discreteReducedCondition
    (reduce_discretePositiveCondition
      (discretePositiveCondition_of_discreteCondition dc)) ;;


(**
The last step consists in translating the obtained unitary condition
into a unitary DNF (Disjunctive Normal Form), that is,
a disjunction of conjunctions of atoms of the form η_x = const.
This is achieved with the classical distributivity laws of Boolean logics:
  x ∧ (y ∨  z) = (x ∧  y) ∨ (x ∧  z)
  (x ∨ y) ∧  z = (x ∧  z) ∨ (y ∧  z)
These transformations preserve the unitary nature of the atoms.

The DNF itself is obtained by applying this process iteratively
until no more change can be made.
Mathematically, this is a fixpoint computation.
The unitary condition is also simplified before the DNF translation.
**)

(*
La dernière étape consiste à transformer la condition unitaire obtenue
en DNF (Disjunctive Normal Form) unitaire, c'est-à-dire en disjonction
de conjonctions d'atomes de la forme η_x = const.
Cela est réalisé à l'aide des lois de distributivité classique
en logique booléenne :
  x ∧ (y ∨  z) = (x ∧  y) ∨ (x ∧  z)
  (x ∨ y) ∧  z = (x ∧  z) ∨ (y ∧  z)
Ces transformations conservent le caractère unitaire des atomes.

La DNF à proprement parler est obtenue en appliquant de façon itérative
ce processus jusqu'à ne plus pouvoir.
En pratique, cela représente un calcul de point fixe.
La condition unitaire est simplifiée avant transformation en DNF.
*)

(** Simplification of a unitary condition **)
let rec simpl_discreteUnitCondition (duc:discreteUnitCondition) =
  let d_simpl_posor c1 c2 =   (* Simplification of PosOr *)
    match simpl_discreteUnitCondition c1 with
      | DUnitBoolean(True) -> DUnitBoolean(True)
      | DUnitBoolean(False) -> simpl_discreteUnitCondition c2
      | cs1 -> (match simpl_discreteUnitCondition c2 with
                  | DUnitBoolean(True) -> DUnitBoolean(True)
                  | DUnitBoolean(False) -> cs1
                  | cs2 -> DUnitOp(PosOr, cs1, cs2)) in
  let d_simpl_posand c1 c2 =    (* Simplification of PosAnd *)
    match simpl_discreteUnitCondition c1 with
      | DUnitBoolean(True) -> simpl_discreteUnitCondition c2
      | DUnitBoolean(False) -> DUnitBoolean(False)
      | cs1 -> (match simpl_discreteUnitCondition c2 with
                  | DUnitBoolean(True) -> cs1
                  | DUnitBoolean(False) -> DUnitBoolean(False)
                  | cs2 -> DUnitOp(PosAnd, cs1, cs2)) in
  let d_simpl_posop op c1 c2 =    (* Call the adequate functions given the operator *)
    match op with
      | PosAnd -> d_simpl_posand c1 c2
      | PosOr -> d_simpl_posor c1 c2 in
  match duc with    (* Simplification of DOp, the others are not reducible *)
    | DUnitOp(posop, duc1, duc2) -> d_simpl_posop posop duc1 duc2
    | _ -> duc ;;   (* DBoolean and DUnitAtomEq do not change *)

(** Distrbution of the disjunction (De Morgan lanw) to make them global **)
let rec distribute = function
  | DUnitOp(PosAnd, x, DUnitOp(PosOr, y, z)) ->
      DUnitOp(PosOr, DUnitOp(PosAnd, x, y), DUnitOp(PosAnd, x, z))
  | DUnitOp(PosAnd, DUnitOp(PosOr, x, y), z) ->
      DUnitOp(PosOr, DUnitOp(PosAnd, x, z), DUnitOp(PosAnd, y, z))
  | DUnitOp(poscomp, x, y) -> DUnitOp(poscomp, distribute x, distribute y)
  | dc -> dc ;;

(** Generic definition of a fixpoint **)
let rec fixpoint x0 func =
  let x = func x0 in
    if x = x0
      then x0
      else fixpoint x func ;;

(** Final result of this part:
    Simplification and translation into a DNF of any discrete condition **)
let dnf (dc:discreteCondition) =
  fixpoint (simpl_discreteUnitCondition (unitarize_discreteCondition dc)) distribute ;;