(**************************)
(*** Standard functions ***)
(**************************)

open Types;;


(* Exception: Not (yet) implemented; used once *)
exception Not_implemented of string ;;

(* nl is inserted after each closing parenthesis
when translating in strings; change into
  let nl = "" ;;
to output one-line conditions, and into
  let nl = "\n" ;;
to spread the conditions on several lines *)
let nl = "" ;;

(* parentheses (or any other symbols) around
a discrete or continuous atom; change into
  let lpatom, rpatom = "(", ")" ;;
to output the parentheses around the atoms, and into
  let lpatom, rpatom = "", "" ;;
to output the atoms without parentheses *)
let lpatom, rpatom = "", "" ;;



(**************************)
(*** Standard functions ***)
(**************************)

(** Identity function **)
let id x = x ;;

(** Function composition **)
let (<<) f1 f2 = fun x -> f2 (f1 x) ;;

(** Evaluation functions **)
let eval_comp = function
  | Eq -> (=)
  | NEq -> (<>)
  | LE -> (<=)
  | LT -> (<)
  | GE -> (>=)
  | GT -> (>) ;;

let eval_op = function
  | And -> (&&)
  | Or -> (||)
  | Impl -> (fun x y -> y || (not x));;

let eval_discreteTermSymbol = function
  | DPlus -> (+)
  | DMinus -> (-) ;;

let eval_termSymbol = function
  | Plus -> (+.)
  | Minus -> (-.)
  | Times -> ( *. )
  | Divide -> (/.) ;;


(** Abstraction functions **)
let boolean_of_bool = function
  | true -> True
  | false -> False ;;