(****************************************************)
(*** Symbols of the languages (syntax, semantics) ***)
(****************************************************)

(** Classical logic connectives *)
type operator =
  | And | Or | Impl ;;
type neg =
  | Neg ;;
type boolean =
  | True | False ;;

(** Comparison connectives *)
type comp =
  | LE | LT | GE | GT | Eq | NEq ;;

(** Symbols + and − on discrete path atoms *)
type symbol =
  | PlusSymbol | MinusSymbol ;;

(** Symbols +, - or nothing on slides *)
type slideSymbol =
  | PlusSlide | MinusSlide | EqSlide ;;

(** Arithmetic connectives for hybrid conditions (+, −, ×, ÷) *)
type termSymbol = 
  | Plus | Minus | Times | Divide ;;

(** Arithmetic connectives for discrete conditions (+, −) *)
type discreteTermSymbol = 
  | DPlus | DMinus ;;


(***************************************)
(*** Types and definition of the GRN ***)
(***************************************)

(** Types **)
type var = string ;;
type mult = string ;;
type resource = mult list;;
type varmax = int ;;
type varpredec = mult list ;;

(** Comparison functions **)
let varequals = (=) ;;


(***********************************************************************)
(*** Grammar and semantics for Hoare logic conditions and assertions ***)
(***********************************************************************)

(** Terms definition **)

type variableExt = string ;;

type time =
  | VarTime of variableExt
  | ValueTime of float ;;

type discreteTerm = 
  | Eta of var
  | DConstant of int
  | DTermRelation of discreteTermSymbol * discreteTerm * discreteTerm

type hybridTerm = 
  | PiEntrance of var * int
  | PiExit of var * int
  | Celerity of var * resource * int
  | HConstant of float
  | HTermRelation of termSymbol * hybridTerm * hybridTerm
  | HTime of time


(** Property language **)

type discreteCondition =
  | DBoolean of boolean
  | DNeg of neg * discreteCondition
  | DAtom of comp * discreteTerm * discreteTerm
  | DOp of operator * discreteCondition * discreteCondition ;;
(* Remark: Discrete atoms are expressed with DAtom *)

type hybridCondition = 
  | HBoolean of boolean
  | HNeg of neg * hybridCondition
  | HAtom of comp * hybridTerm * hybridTerm
  | HDAtom of comp * discreteTerm * discreteTerm
  | HOp of operator * hybridCondition * hybridCondition ;;
(* Remark: Hybrid atoms are expressed with HAtom
           and discrete atoms are expressed with HDAtom *)

(** Property **)

type property = discreteCondition * hybridCondition ;;

(** Extraction of the discrete or hybrid part of a property **)
let discretePart (p:property) = fst p ;;
let hybridPart (p:property) = snd p ;;

(** Assertion language **)
type assertion =
  | ABoolean of boolean
  | ACelerity of comp * var * hybridTerm
  | ASlide of slideSymbol * var 
  | ANoSlide of slideSymbol * var 
  | AOp of operator * assertion * assertion
  | ANeg of neg * assertion ;;

type multformula =
  | MAtom of var * int
  | MPropUn of neg * multformula
  | MPropBin of operator * multformula * multformula
(* Remark: Multiplex atoms understate a GE comparator *)

(** Path language **)
type dpa = var * symbol ;;

type pathLanguage =
  | Nothing
  | Tuple of time * assertion * dpa
  | Seq of pathLanguage * pathLanguage ;;

type postcondition =
	| Postcondition of discreteCondition * hybridCondition

type informationVar = int * string list;;
type vars = string * informationVar;;
type varlist = vars list;;

type mults = string * multformula;;
type multlist = mults list;;
type vardef = varlist * multlist;;

type newElt =
	| HoareTriple of pathLanguage * postcondition
	| InfluenceGraph of vardef
  | IsCyclic of bool
;;


