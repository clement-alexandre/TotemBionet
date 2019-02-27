(* File lexer.mll *)
{
  open Parser_dinit       (* The type token is defined in parser.mli *)
  exception Eof
  let line_num = ref 1
  exception Syntax_error of string

  let syntax_error msg = raise(Syntax_error (msg ^ " on line " ^ (string_of_int !line_num)))
}

let blank = [' ' '\r' '\t' '\n']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let digits = digit+



(* Language property *)
let eta = ("eta"|"Eta")


rule token = parse
  blank                                               { token lexbuf }     (* skip blanks *)
  | '('                                               { LPAREN }
  | ')'                                               { RPAREN }
        (* Postcondition : Property Language (D) *)
  | eta                                               { ETA }
  | '='                                               { EQ }
  | digits as d                                       { INT(d) }
  | alpha+ as a                                       { VAR(a) }
  | eof                                               { raise Eof }
  | _                                                 { syntax_error "couldn't identify the token" }
