(*****************************************)
(*** Grammatical syntax for the parser ***)
(*****************************************)

(* This file defines the grammatical syntax of the input file.
This grammar is recognized by the lexer, and a rule is applied
depending on the grammar *)

{
  open Parser_parameters        (* The type token is defined in parser.mli *)
  exception Eof
  let line_num = ref 1
  exception Syntax_error of string

  let syntax_error msg = raise(Syntax_error (msg ^ " on line " ^ (string_of_int !line_num)))
}

(* Classical grammar *)
let blank = [' ' '\r' '\t' '\n']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let char = ['.' '!' ''' '"' '#' '~' ';' ':' ',' '%' '/' '-' '_' '@' '^' '$' '*' '?' '<' '>' '=' '&' '{' '}' '(' ')' '[' ']' '|' '\\']
let digits = digit+
let integer = '-'? digits
let realnumber = '-'? digits '.' digit*
let rational = '-'? digits '/' digits



let mult = 'm' digits
let mults = mult? ("_" mult)*

let cel = "C_" alpha+ "__" mults "__" digits


(* Definition of the grammar observed into the input file *)
rule token = parse
  blank                                               { token lexbuf }     (* skip blanks *)
  | '\n'                                              { EOL }
  | '='                                               { EQ }
  | ','                                               { COMMA }
  | ';'                                               { SEMICOLON }
  | '['                                               { LSBRACKET }
  | ']'                                               { RSBRACKET }
  | cel as c                                          { CEL(c) }
  | realnumber as r                                   { REALNUMBER(r) } 
  | rational as ra                                    { RATIONAL(ra) }
  | integer as i                                      { INT(i) }
  | eof                                               { raise Eof }
  | _                                                 { syntax_error "couldn't identify the token" }
