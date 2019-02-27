(*********************************)
(*** Display of types defined  ***)
(*********************************)

open Types;;

(* DEBUG: nl is inserted after each closing parenthesis
when translating in strings; change into
  let nl = "" ;;
to output one-line conditions, and into
  let nl = "\n" ;;
to spread the conditions on several lines *)
let nl = "" ;;

(* DEBUG: parentheses (or any other symbols) around
a discrete or continuous atom; change into
  let lpatom, rpatom = "(", ")" ;;
to output the parentheses around the atoms, and into
  let lpatom, rpatom = "", "" ;;
to output the atoms without parentheses *)
let lpatom, rpatom = "", "" ;;

(** Identity function **)
let id x = x ;;
(** Translations into string for printing **)
let string_of_var (v:var) = id v ;;
let string_of_mult (m:mult) = id m ;;
let string_of_variableExt (x:variableExt) = (id x) ;;

let boolean_of_string s =
	match s with
		| "true"|"True"|"TRUE" -> True
		| "false"|"False"|"FALSE" -> False
		| _ -> raise(Invalid_argument "boolean_of_string: should be a string representing bool type!")
;;

(** Translate a couple into a string given two translation functions *)
let string_of_couple conv1 conv2 sep delim1 delim2 couple =
  delim1 ^ (conv1 (fst couple)) ^ sep ^ (conv2 (snd couple)) ^ delim2
;;

(** Translate an option into a string given a translation function *)
(* let string_of_option conv = function
  | None -> "None"
  | Some x -> "Some " ^ (conv x)
;; *)

(** Translate a list into a string given a translation function and a seprarator *)
let rec string_of_list conv sep l =
  match l with
    | [] -> ""
    | h :: [] -> (conv h)
    | h :: t -> ((conv h) ^ sep ^ (string_of_list conv sep t))
;;

(** Translate a list into a string given a translation function and a seprarator *)
let rec string_of_tuple_list conv sep l sep_tuple =
  match l with
    | [] -> ""
    | h :: [] -> "\t" ^ ((conv (fst h)) ^ sep_tuple ^ conv (snd h))
    | h :: t -> "\t" ^ (((conv (fst h)) ^ sep_tuple ^ conv (snd h)) ^ sep ^ (string_of_tuple_list conv sep t sep_tuple))
;;

let string_of_list_delim conv sep delim1 delim2 l =
  delim1 ^ string_of_list conv sep l ^ delim2
;;

(* let rec string_of_list_mult conv sep l =
  match l with
    | [] -> ""
    | h :: [] -> "\"" ^ (conv h) ^ "\""
    | h :: t -> ("\"" ^ (conv h) ^ "\"" ^ sep ^ (string_of_list_mult conv sep t))
;; *)

(** Translation into strings for printing **)
let string_of_operator = function
  | And -> "And"
  | Or -> "Or"
  | Impl -> "Impl"
;;

let string_of_operator_sv = function
  | And -> "∧"
  | Or -> "∨"
  | Impl -> "⇒"
;;

let string_of_neg = function
  | Neg -> "Neg"
;;

let string_of_neg_sv = function
  | Neg -> "¬"
;;

let string_of_boolean = function
  | True -> "True"
  | False -> "False"
;;

let string_of_comp = function
  | LE -> "LE"
  | LT -> "LT"
  | GE -> "GE"
  | GT -> "GT"
  | Eq -> "Eq"
  | NEq -> "NEq"
;;

let string_of_comp_sv = function
  | LE -> "≤"
  | LT -> "<"
  | GE -> "≥"
  | GT -> ">"
  | Eq -> "="
  | NEq -> "≠"
;;

let string_of_symbol = function
  | PlusSymbol -> "PlusSymbol"
  | MinusSymbol -> "MinusSymbol"
;;

let string_of_slideSymbol = function
  | PlusSlide -> "PlusSlide"
  | MinusSlide -> "MinusSlide"
  | EqSlide -> "EqSlide"
;;

(* let string_of_termSymbol = function
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Times -> "Times"
  | Divide -> "Divide"
;; *)

let string_of_termSymbol_sv = function
  | Plus -> "+"
  | Minus -> "−"
  | Times -> "×"
  | Divide -> "÷"
;;

let string_of_discreteTermSymbol = function
  | DPlus -> "DPlus"
  | DMinus -> "DMinus"
;;

let string_of_time = function
  | VarTime x -> string_of_variableExt x
  | ValueTime f -> string_of_float f
;;

let rec string_of_discreteTerm = function
  | Eta v -> "Eta(\"" ^ (string_of_var v) ^ "\")"
  | DConstant i -> "DConstant(" ^ (string_of_int i) ^")"
  | DTermRelation (symb, dt1, dt2) -> "(" ^ (string_of_discreteTerm dt1) ^
      " " ^ (string_of_discreteTermSymbol symb) ^ " " ^ (string_of_discreteTerm dt2) ^ ")"
;;

let rec string_of_hybridTerm = function
  | PiEntrance (v, i) -> "π_" ^ (string_of_var v) ^ "^" ^ (string_of_int i)
  | PiExit (v, i) -> "π'_" ^ (string_of_var v) ^ "^" ^ (string_of_int i)
  | Celerity (v, omega, n) -> "C_" ^ (string_of_var v) ^ "," ^
      (string_of_list_delim string_of_mult "," "{" "}" omega) ^ "," ^
      (string_of_int n)
  | HConstant f -> (string_of_float f)
  | HTermRelation (symb, ht1, ht2) -> (string_of_hybridTerm ht1) ^
      " " ^ (string_of_termSymbol_sv symb) ^ " " ^ (string_of_hybridTerm ht2)
  | HTime t -> string_of_time t
;;

let rec string_of_discreteCondition = function
  | DBoolean b -> (string_of_boolean b) ^ nl
  | DNeg (n, dc) -> (string_of_neg_sv n) ^ (string_of_discreteCondition dc)
  | DAtom (c, dt1, dt2) -> lpatom ^ (string_of_discreteTerm dt1) ^
      (string_of_comp_sv c) ^ (string_of_discreteTerm dt2) ^ rpatom ^ nl
  | DOp (op, dc1, dc2) -> "(" ^ (string_of_discreteCondition dc1) ^ " " ^
      (string_of_operator_sv op) ^ " " ^ (string_of_discreteCondition dc2) ^ ")" ^ nl
;;

let rec string_of_hybridCondition = function
  | HBoolean b -> (string_of_boolean b) ^ nl
  | HNeg (n, hc) -> (string_of_neg_sv n) ^ "(" ^ (string_of_hybridCondition hc) ^ ")"
  | HAtom (c, ht1, ht2) -> lpatom ^ (string_of_hybridTerm ht1) ^
      (string_of_comp_sv c) ^ (string_of_hybridTerm ht2) ^ rpatom ^ nl
  | HDAtom (c, dt1, dt2) -> string_of_discreteCondition (DAtom(c, dt1, dt2))
  | HOp (op, hc1, hc2) -> "(" ^ (string_of_hybridCondition hc1) ^ " " ^
      (string_of_operator_sv op) ^ " " ^ (string_of_hybridCondition hc2) ^ ")" ^ nl
;;


(* let string_of_property = function
  | (dc, hc) -> "( " ^ (string_of_discreteCondition dc) ^ " ; " ^
      (string_of_hybridCondition hc) ^ " )"
;; *)

let rec string_of_assertion = function
  | ABoolean b -> string_of_boolean b
  | ACelerity (c, v, ht2) -> "C_" ^ (string_of_var v) ^ (string_of_comp c) ^
      (string_of_hybridTerm ht2)
  | ASlide (slsymb, v) -> "slide" ^ (string_of_slideSymbol slsymb) ^
      "(" ^ (string_of_var v) ^ ")"
  | ANoSlide (slsymb, v) -> "noslide" ^ (string_of_slideSymbol slsymb) ^
      "(" ^ (string_of_var v) ^ ")"
  | AOp (op, a1, a2) -> "(" ^ (string_of_assertion a1) ^ " " ^
      (string_of_operator op) ^ " " ^ (string_of_assertion a2) ^ ")"
  | ANeg (n, a) -> (string_of_neg n) ^ (string_of_assertion a)
;;

(* let string_of_dpa = function
  | (v, symb) -> "(\"" ^ (string_of_var v) ^ "\"," ^ (string_of_symbol symb) ^ ")"
;;

let rec string_of_pathLanguage = function
  | Nothing -> ""
  | Tuple (t, a, p) -> "(" ^ (string_of_time t) ^ ", " ^
      (string_of_assertion a) ^ ", " ^ (string_of_dpa p) ^ ")"
  | Seq (p1, p2) -> (string_of_pathLanguage p1) ^ " ; " ^
      (string_of_pathLanguage p2) *)
;;


(*************************************************************************)
(*** Display HybridConjunction, HybridDisjunction and lvaluation lists ***)
(*************************************************************************)

(** Translation of a hybridConj into a string **)
let string_of_hybridConj hconj =
  string_of_list string_of_hybridCondition " ∧ " hconj ;;

(** Translation of a hybridDisj into a string **)
let string_of_hybridDisj hdisj =
  string_of_list string_of_hybridCondition " ∨ " hdisj ;;

let string_of_valuation v =
  string_of_hybridTerm (fst v) ^ "=" ^ string_of_float (snd v)
;;

let rec string_of_lval l =
  match l with
    | [] -> ""
    | h::[] -> string_of_valuation h
    | h::t -> (string_of_valuation h) ^ " ∧ " ^ (string_of_lval t)
;;











let string_of_comp_solver = function
  | LE -> "<="
  | LT -> "<"
  | GE -> ">="
  | GT -> ">"
  | Eq -> "="
  | NEq -> "!="
;;

let string_of_termSymbol_solver = function
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"
;;

let string_of_neg_solver = function
  | Neg -> "!"
;;

let rec string_of_hybridTerm_solver = function
  | PiEntrance (v, i) -> "pi_entrance_" ^ (string_of_var v) ^ "_" ^ (string_of_int i)
  | PiExit (v, i) -> "pi_exit_" ^ (string_of_var v) ^ "_" ^ (string_of_int i)
  | Celerity (v, omega, n) -> "C_" ^ (string_of_var v) ^
      (string_of_list_delim string_of_mult "_" "__" "__" omega) ^
      (string_of_int n)
  | HConstant f -> (string_of_float f)
  | HTermRelation (symb, ht1, ht2) -> (string_of_hybridTerm_solver ht1) ^
      " " ^ (string_of_termSymbol_solver symb) ^ " " ^ (string_of_hybridTerm_solver ht2)
  | HTime t -> string_of_time t
;;

let rec string_of_hybridCondition_solver = function
  | HBoolean(b) -> (string_of_boolean b)
  | HNeg(n, hc) -> (string_of_neg_solver n) ^ "(" ^ (string_of_hybridCondition_solver hc) ^ ")"
  | HAtom(c, ht1, ht2) -> (string_of_hybridTerm_solver ht1) ^
      (string_of_comp_solver c) ^ (string_of_hybridTerm_solver ht2) 
  | HDAtom(c, dt1, dt2) -> string_of_discreteCondition (DAtom(c, dt1, dt2))
  | HOp(And, hc1, hc2) -> "(" ^ (string_of_hybridCondition_solver hc1) ^ " && " ^ (string_of_hybridCondition_solver hc2) ^ ")"
  | HOp(Or, hc1, hc2) -> "(" ^ (string_of_hybridCondition_solver hc1) ^ " || " ^ (string_of_hybridCondition_solver hc2) ^ ")"
  | _ -> raise(Invalid_argument("string_of_hybridCondition_solver: error or operator 'Or' found! "))
;;


(******************************)
(*** Translation into Latex ***)
(******************************)
(* 
let latex_of_boolean = function
  | True -> "\\top"
  | False -> "\\bot";;

let latex_of_comp = function
  | LE -> "\\leq"
  | LT -> "<"
  | GE -> "\\geq"
  | GT -> ">"
  | Eq -> "="
  | NEq -> "\\neq";;

let latex_of_op = function
  | And -> "\\wedge"
  | Or -> "\\vee"
  | Impl -> "\\Rightarrow";;

let latex_of_resources omega =
  if ((=) omega []) then (string_of_list id "," ["\\emptyset"]) else ("\\{" ^ (string_of_list id "," omega) ^ "\\}");;

let latex_of_termSymbol = function 
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "\\times"
  | Divide -> "\\";;

let latex_of_time = function 
  | VarTime(v) -> v  ^ " "
  | ValueTime(f) -> (string_of_float f) ^ " ";;

let rec latex_of_discreteTerm = function
  | Eta(v) -> "\\eta_" ^ v ^ " "
  | DConstant(i) -> (string_of_int i) ^ " "
  | DTermRelation(s,d1,d2) -> "(" ^ (latex_of_discreteTerm d1) ^ " " ^ (string_of_discreteTermSymbol s) ^ " " ^ (latex_of_discreteTerm d2) ^ ") ";;

let rec latex_of_discreteCondition = function
  | DBoolean(b) -> (latex_of_boolean b) ^ " "
  | DNeg(n,d) -> "\\lnot" ^ (latex_of_discreteCondition d) ^ " "
  | DAtom(c,d1,d2) -> "(" ^ (latex_of_discreteTerm d1) ^ " " ^ (latex_of_comp c) ^ " " ^ (latex_of_discreteTerm d2) ^ ") "
  | DOp(op,d1,d2) -> "(" ^ (latex_of_discreteCondition d1) ^ " " ^ (latex_of_op op) ^ " " ^ (latex_of_discreteCondition d2) ^ ") ";;

let rec latex_of_hybridTerm = function
  | PiEntrance(v,i) -> "{\\pi_{" ^ v ^ "}^{" ^ (string_of_int i) ^ "}}'"
  | PiExit(v,i) -> "\\pi_{" ^ v ^ "}^{" ^ (string_of_int i) ^ "}"
  | Celerity(v,omega,i) -> "C_{" ^ v ^ "," ^ (latex_of_resources omega) ^ "," ^ (string_of_int i) ^ "}"
  | HConstant(f) -> if ( (=) (ceil f) f) then (string_of_int (int_of_float f)) else ((string_of_float f))
  | HTermRelation(s,h1,h2) -> "(" ^ (latex_of_hybridTerm h1) ^ " " ^ (latex_of_termSymbol s) ^ " " ^ (latex_of_hybridTerm h2) ^ ")"
  | HTime(t) -> (latex_of_time t);;

let rec latex_of_hybridCondition = function
  | HBoolean(b) -> (latex_of_boolean b) ^ " "
  | HNeg(n,h) ->  "\\lnot" ^ (latex_of_hybridCondition h) ^ " "
  | HAtom(c,h1,h2) -> "(" ^ (latex_of_hybridTerm h1) ^ " " ^ (latex_of_comp c) ^ " " ^ (latex_of_hybridTerm h2) ^ ")"
  | HDAtom(c,d1,d2) -> "(" ^ (latex_of_discreteTerm d1) ^ " " ^ (latex_of_comp c) ^ " " ^ (latex_of_discreteTerm d2) ^ ")"
  | HOp(op, h1, h2) -> "(" ^ (latex_of_hybridCondition h1) ^ " " ^ (latex_of_op op) ^ " " ^ (latex_of_hybridCondition h2) ^ ")";;
 *)




