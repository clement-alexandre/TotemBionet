(*****************************************************************************)
(*** WP computation of the hybrid condition with on-the-fly simplification ***)
(*****************************************************************************)

open Types;;
open Extraction_data;;
open Standard_functions;;
open Set_states;;
open Display;;

(**
this part aims at computing the hybrid weakest precondition
for a given program and postcondition,
using the list of states corresponding to the discrete strongest postcondition
(see previous part).

The discrete strongest postcondition allows to know the set of
relevant discrete states and perform on-the-fly simplification:
this hybrid weakest precondition is computed for each of these candidate
final states and, the discrete levels and resources being thus known,
this reduced a lot the size of the final formulas.
These simplifications are directly integrated into the functions
implementing the different sub-formulas of the weakest precondition.
**)

(*
Cette partie a pour objectif de calculer la plus faible précondition hybride
pour un programme et une postcondition hybride donnés,
à l'aide de la liste des états correspondant à la plus forte postcondition discrète
(voir partie précédente).

La plus forte postcondition permet de connaître la liste des états discrets
pertinents et de pratiquer une simplification à la volée :
la plus faible condition hybride est calculée pour chacun de ces états discrets,
et, les niveaux discrets et les ressources étant alors connus,
cela réduit grandement la taille des formules.
Les réductions sont directement intégrées dans les fonctions codant les
différentes sous-formules.
*)

(** Compute the index of variable x in list varlist **)
let rec indexOf (x:var) =
  let rec value_index l x =
    match l with
    | [] -> raise (Invalid_argument "empty_indexOf: no variable defined")
    | h::[] -> if (varequals h x) then (0) else raise (Invalid_argument "missing_indexOf: variable is missing")
    | h::t -> if (varequals h x) then (0) else (1 + (value_index t x))
  in value_index varlist x;;

(** Extract the value of Eta(x) for x in a given state (list) **)
let rec value_eta_var (l:state) (x:var) =
  List.nth l (indexOf x);;

(** Compute the neighbour state of s where v is increased/decreased given pm **)
let neighbour_state (s:state) (v:var) (pm:symbol) =
  let eval_symbol = function
    | PlusSymbol -> (+)
    | MinusSymbol -> (-) in
  let rec rec_neighbour_state (cs:state) (cvarlist:var list) =
    match cs, cvarlist with
      | hcs :: tcs, hcv :: tcv ->
        (if varequals v hcv
          then ((eval_symbol pm) hcs  1) :: tcs
          else hcs :: (rec_neighbour_state tcs tcv))
      | _, _ -> raise Not_found in
  rec_neighbour_state s varlist ;;

(** Compute the resources of a variable for a given state **)
let eval_resource_omega (l:state) (x:var) =
      (* Check if the multiplex formula is satisfied in state l *)
  let rec compare_eta_and_formula_mult l f =
    match f with
    | MAtom(v,n) -> (eval_comp GE) (value_eta_var l v) n
    | MPropUn(neg,m) -> not (compare_eta_and_formula_mult l m)
    | MPropBin(op,m1,m2) -> (eval_op op) (compare_eta_and_formula_mult l m1) (compare_eta_and_formula_mult l m2)
  in List.filter (fun m -> compare_eta_and_formula_mult l (getformula(m))) (getpredec(x));;

let eval_resource_omega_varlist (l:state) =
  let rec eval_each_var l vl lf =
    match vl with
      | [] -> lf
      | h :: t -> eval_each_var l t ([eval_resource_omega l h] @ lf)
  in List.rev (eval_each_var l varlist []);;

(** Formulas Phi_v^+(∆t) and Phi_v^−(∆t) (v can increase/decrease)
    Transformation of the sign (+/−) into a comparator (>/<) **)
let calculPhiSigned variable etaVariable omega sign time index = 
  match sign with
    | PlusSymbol -> HOp(
      And, 
      HAtom(Eq, PiExit(variable, index), HConstant(1.0)),
      HOp(
        And, 
        HAtom(GT, Celerity(variable, omega, etaVariable), HConstant(0.)), 
        HAtom(
          Eq, 
          PiEntrance(variable ,index), 
          HTermRelation(
            Minus, 
            PiExit(variable, index), 
            HTermRelation(
              Times, 
              (Celerity(variable, omega, etaVariable)), 
              time)))))
    | MinusSymbol -> HOp(
      And, 
      HAtom(Eq, PiExit(variable, index), HConstant(0.0)), 
      HOp(
        And, 
        HAtom(LT, Celerity(variable, omega, etaVariable), HConstant(0.)), 
        HAtom(
          Eq, 
          PiEntrance(variable, index), 
          HTermRelation(
            Minus, 
            PiExit(variable, index), 
            HTermRelation(
              Times, 
              (Celerity(variable, omega, etaVariable)), 
              time)))));;

(** Portion of J_v concerning variables other than v
    Conjonctions pi_u^f = pi'_u^i on all variables (v already excluded) **)
let rec link_pi l i =
  match l with
    | [] -> raise (Invalid_argument "link_pi: Empty varlist (should contain at least one variable)")
    | h::[] -> HAtom(
              Eq,
              PiExit(h,(i+1)),
              PiEntrance(h,(i)))
    | h::t -> HOp(
            And, 
            HAtom(
              Eq,
              PiExit(h,(i+1)),
              PiEntrance(h,(i))),
            link_pi t i) ;;

(** Complete formula J_v (junction around a discrete transition of v) **)
let calcul_dj_param variable index =
  HOp(
    And, 
    HAtom(
      Eq, 
      PiExit(variable, index), 
      HTermRelation(
        Minus, 
        HConstant(1.0),
        PiEntrance(variable, (index - 1)))),
    link_pi (List.filter ((<>) variable) varlist) (index - 1)) ;;


let add_cj var i omega eta time =
  HOp(
    And,
    HOp(
      Impl,
      HAtom(
        Eq,
          PiExit(var,i),
          HConstant(1.)),
      HOp(
        And,
        HAtom(
          GT,
          Celerity(var, omega, eta),
          HConstant(0.)),
        HAtom(
          GE,
          PiEntrance(var,i),
          HTermRelation(
            Minus,
            PiExit(var,i),
            HTermRelation(
              Times,
              Celerity(var, omega, eta),
              time))))),
    HOp(
      And,
      HOp(
        Impl,
        HAtom(
          Eq,
          PiExit(var,i),
          HConstant(0.)),
        HOp(
          And,
          HAtom(
            LT,
            Celerity(var, omega, eta),
            HConstant(0.)),
          HAtom(
            LE,
            PiEntrance(var,i),
            HTermRelation(
              Minus,
              PiExit(var,i),
              HTermRelation(
                Times,
                Celerity(var, omega, eta),
                time))))),
      HOp(
        Impl,
        HOp(
          And,
          HAtom(
            GT,
            PiExit(var,i),
            HConstant(0.)),
          HAtom(
            LT,
            PiExit(var,i),
            HConstant(1.))),
        HAtom(
          Eq,
          PiEntrance(var,i),
          HTermRelation(
            Minus,
            PiExit(var,i),
            HTermRelation(
              Times,
              Celerity(var, omega, eta),
              time))))))
;;

let rec calcul_cj_param lvar i state time =
  match lvar with
    | [] -> raise(Invalid_argument "calcul_cj_param: none variable defined in the influence graph")
    | h :: [] -> add_cj h i (eval_resource_omega state h) (value_eta_var state h) time
    | h :: t -> HOp(And, (add_cj h i (eval_resource_omega state h) (value_eta_var state h) time), (calcul_cj_param t i state time))
;;


let calcul_j_param variable index state t =
  HOp(
    And,
    calcul_cj_param varlist index state t,
    calcul_dj_param variable index)

(** Generic part of formulas EW_u^+ and EW_u^− (external walls)
    n = 0 or b_u; op = > or < **)
let calcul_ew_param v etaVar omega valueBoundary op =
  if ((=) etaVar valueBoundary )
  then (HAtom(op, Celerity(v, omega, etaVar), HConstant(0.0)))
  else (HBoolean(False));;

(** Generic part of formulas IW_u^+ and IW_u^− (internal walls)
    op = > or <; sign = + or −; n = 0 or b_v **)
(* This function contains a usual simplification that comes from
axioms of the hybrid Thomas modelling:
  C_u,w,n > 0 ∧ C_u,w,n±1 < 0
  C_u,w,n < 0 ∧ C_u,w,n±1 > 0
both simplify into:
  False
because two celerities cannot have a different sign
if they have the same resources and are neighbour (difference of 1 on n). *)
let calcul_iw_param v etaVar omega omegaNeighbor boundary sign =
      (* Sub-part common to formulas IW_u^+ and IW_u^− *)
  let calcul_iw_param_sign comp1 comp2 addsub =
    if ((eval_comp comp2) etaVar boundary)
      && (omega <> omegaNeighbor)   (* Simplify (axioms of the modelling) *)
        then HOp(And,
          HAtom(comp1, Celerity(v, omega, etaVar), HConstant(0.0)),
          HAtom(comp2, Celerity(v, omegaNeighbor, (addsub etaVar 1)), HConstant(0.0)))
        else HBoolean(False) in
  match sign with
    | PlusSymbol -> calcul_iw_param_sign GT LT (+)
    | MinusSymbol -> calcul_iw_param_sign LT GT (-) ;;

(** Formulas W_u^+ et W_u^− (internal and external walls); calls EW and IW **)
let calcul_w_param variable etaVar omega omegaNeighbor sign =
  match sign with
    | PlusSymbol -> HOp(Or,
                        calcul_iw_param variable etaVar omega omegaNeighbor (getbound variable) PlusSymbol,
                        calcul_ew_param variable etaVar omega (getbound variable) GT)    
    | MinusSymbol -> HOp(Or,
                         calcul_iw_param variable etaVar omega omegaNeighbor 0 MinusSymbol,
                       calcul_ew_param variable etaVar omega 0 LT) ;;

(** Formula First_v(∆t) (v can first change its discrete state) **)
let calculFirst variable state time index =
      (* Generic part of First_v(∆t) (external conjunction on u) *)
  let calculFirstByVariable var omega etaVar =
        (* Generic part of First_v(∆t) (internal conjunctions for W^+/−) *)
    let sign_calculFirstByVariable comp symb =
      HOp(Impl,
        HOp(And,
          HAtom(comp, Celerity(var, omega, etaVar), HConstant(0.)),
          HAtom(comp, PiEntrance(var, index),
            HTermRelation(Minus,
              PiExit(var, index),
              HTermRelation(Times, Celerity(var, omega, etaVar), time)))),
        (calcul_w_param var etaVar omega
          (eval_resource_omega (neighbour_state state var symb) var) symb)) in
    HOp(And,
      sign_calculFirstByVariable GT PlusSymbol,
      sign_calculFirstByVariable LT MinusSymbol) in
      (* Piece of condition in conjunction *)
  let this_calculFirstByVariable (v:var) =
    calculFirstByVariable v (eval_resource_omega state v) (value_eta_var state v) in
      (* Iteration on each variable (except the one that crosses threshold) *)
  let rec iter_v l =
    match l with
      | [] -> HBoolean(True)
      | h::[] -> this_calculFirstByVariable h
      | h::t -> HOp(And, this_calculFirstByVariable h, iter_v t) in
  iter_v (List.filter ((<>) variable) varlist) ;;

(** Formulas Sl_u,omega_u,n_u^+/−/=(∆t) (slides) for Assert **)
(* s:slideSymbol v:var w:omega n:niveau discret t:time i:indice *)
let rec calculSlide var omega etaVar time symbol index =
  let generic_slide a1 a2 =   (* Generic formula of Sl^+/− *)
    HOp(And,
        HAtom(Eq, PiExit(var,index), HConstant(a1)),
        HAtom(a2,
                  PiEntrance(var,index),
                  HTermRelation(Minus,
                                PiExit(var,index),
                                HTermRelation(Times, Celerity(var,omega,etaVar), time))))
  in match symbol with   (* Final formula: depending on the sign *)
      | PlusSlide -> generic_slide 1.0 GT
      | MinusSlide -> generic_slide 0.0 LT
      | EqSlide -> HOp(Or, calculSlide var omega etaVar time PlusSlide index, calculSlide var omega etaVar time MinusSlide index) ;;

let rec calculnoSlide var omega etaVar time symbol index =
  let generic_noslide comp1 var index fl comp2 omega etaVar comp3 time = (* Generic formula of noSl^+/− *)
    HOp(
      And,
      HAtom(
        comp1,
        PiExit(var,index),
        HConstant(fl)),
      HOp(
        Impl,
        HAtom(
          comp2,
          Celerity(var,omega, etaVar),
          HConstant(0.0)),          
        HAtom(
          comp3,
          PiEntrance(var,index),
          HTermRelation(
            Minus,
            PiExit(var,index),
            HTermRelation(
              Times,
              Celerity(var,omega,etaVar),
              time)))))
  in match symbol with
      | PlusSlide -> generic_noslide LT var index 1.0 GT omega etaVar LE time
      | MinusSlide -> generic_noslide GT var index 0.0 LT omega etaVar GE time
      | EqSlide -> HOp(And, calculnoSlide var omega etaVar time PlusSlide index, calculnoSlide var omega etaVar time MinusSlide index) ;;

(** Formula Assert(∆t) (hybrid assertions) **)
let calculAssert a state time index =
  let rec replaceAssert a state time index =   (* Translate at the lowest of the enumerations *)
    match a with
      | ABoolean(b) -> HBoolean(b)
      | ACelerity(comp,v,h1) -> HAtom(comp,Celerity(v,(eval_resource_omega state v), (value_eta_var state v)), h1)
      | ASlide(sign,v) -> (calculSlide v (eval_resource_omega state v) (value_eta_var state v) time sign index)
      | ANoSlide(sign,v) -> (calculnoSlide v (eval_resource_omega state v) (value_eta_var state v) time sign index)
      | AOp(op,a1,a2) -> HOp(op, (replaceAssert a1 state time index), (replaceAssert a2 state time index))
      | ANeg(n,a1) -> HNeg(n, (replaceAssert a1 state time index))
    in replaceAssert a state time index ;;


(**
This part computes the hybrid part of the weakest precondition
depending on the program and the set of states of the strongest postcondition.
The precondition takes the form of a list of couples because
to each state of the strongest postcondition corresponds
a part of the hybrid precondition.
The elements of this list are virtually in disjunction.
**)

(*
Cette partie calcule la partie hybride de la plus faible précondition
à partir du programme et de l'ensemble des états de la plus forte postcondition.
La précondition prend la forme d'une liste car à chaque état en postcondition
correspond une partie de la précondition.
Les éléments de la liste sont virtuellement en disjonction.
*)

(** Type lcondition: list of (state, hybridcondition) corresponding to
    a property (discrete state and corresponding hybrid condition) **)
type lcondition = (state * hybridCondition) list ;;


(** Formula of the hybrid weakest precondition
    for a unique v+/v− instruction (and information of time and assert)
    and a given postcondition state **)
let calculHCondition (post:hybridCondition) (var:var) (sign:symbol)
    (state:state) (a:assertion) (time:hybridTerm) (index:int) =
  HOp(
    And,
    post,
    HOp(
      And,
      calculPhiSigned var (value_eta_var state var) (eval_resource_omega state var) sign time index,
      HOp(
        And,
        calculFirst var state time index,
        HOp(
          And,
          HNeg(
            Neg,
            calcul_w_param var (value_eta_var state var) (eval_resource_omega state var) (eval_resource_omega (neighbour_state state var sign) var) sign),
          HOp(
            And,
            calculAssert a state time index,
            calcul_j_param var index state time)))));;

(** 
Compute the hybrid weakest precondition
for a unique v+/v− instruction (and information of time and assert)
by enumerating all states in the postcondition 
**)
let rec calculHConditionByState (lpost:lcondition) (var:var)
    (sign:symbol) (a:assertion) (time:hybridTerm) (index:int) =
      (* Inversion of sign +/− *)
  let invert_symbol s =
    match s with
    | PlusSymbol -> MinusSymbol
    | MinusSymbol -> PlusSymbol in
      (* Recursion on all disjunctions (state, hybirdCondition) of the postcondition *)
  match lpost with
    | [] -> []
    | (s, hc) :: t -> let prev_s = neighbour_state s var (invert_symbol sign) in
        (prev_s, calculHCondition hc var sign prev_s a time index)
          :: (calculHConditionByState t var sign a time index) ;;


(** Compute the hybrid weakest precondition **)
let hybridWP (prog:pathLanguage) (hpost:hybridCondition) (lstate:state list) =
      (* Combine a list with always the same element *)
  let rec pseudo_combine (l:state list) (a:hybridCondition) =
    match l with
      | [] -> []
      | h :: t -> (h, a) :: (pseudo_combine t a) in
      (* Recursion on the whole program (backward strategy) *)
  let rec calculWPbyStep (prog:pathLanguage) (lpost:lcondition) (index:int) =
    match prog with
    | Nothing -> (lpost, 0)
    | Tuple(time, a, dpa) ->
        (calculHConditionByState lpost (varDpa dpa) (signDpa dpa) a (HTime(time)) index,
        index + 1)
    | Seq(p1, p2) ->
        let (reslpost2, resindex2) = calculWPbyStep p2 lpost index in
          calculWPbyStep p1 reslpost2 resindex2 in
  fst (calculWPbyStep prog (pseudo_combine lstate hpost) 1);;
