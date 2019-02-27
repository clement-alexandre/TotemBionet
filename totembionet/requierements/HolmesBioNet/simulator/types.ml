(*****************************************************************)
(*** Symbols of the languages (syntax, semantics and printing) ***)
(*****************************************************************)

(** Classical logic connectives *)
type operator =
  | And | Or | Impl ;;
type neg =
  | Neg ;;


(***************************************)
(*** Types and definition of the GRN ***)
(***************************************)

(** Types **)
type var = string ;;

(** Comparison functions **)
let varequals = (=) ;;
(* let multcompare = compare ;; *)


(*****************************************************)
(*** Grammar and semantics for the Influence Graph ***)
(*****************************************************)


type multformula =
  | MAtom of var * int
  | MPropUn of neg * multformula
  | MPropBin of operator * multformula * multformula
(* Remark: Multiplex atoms understate a GE comparator *)

type informationVar = string * string list;;
type vars = string * informationVar;;
type varlist = vars list;;

type mults = string * multformula;;
type multlist = mults list;;
type vardef = varlist * multlist;;

type newLine =
	| InfluenceGraph of vardef
;;


(***************************************)
(*** Types for parsing of Celerities ***)
(***************************************)
type fileLine = 
	IdentifyCelValue of string * string
