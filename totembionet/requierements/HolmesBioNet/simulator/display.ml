open Types;;
(** Translation into strings for printing **)
let string_of_operator = function
  | And -> "And"
  | Or -> "Or"
  | Impl -> "Impl" ;;

let string_of_neg = function
  | Neg -> "Neg" ;;



