(* File lexer.mll *)
{
  open Parser_cel        (* The type token is defined in parser.mli *)
  exception Eof
  let line_num = ref 1
  exception Syntax_error of string

  let syntax_error msg = raise(Syntax_error (msg ^ " on line " ^ (string_of_int !line_num)))
}

let blank = [' ' '\r' '\t' '\n']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let char = ['.' '!' ''' '"' '#' '~' ';' ':' ',' '%' '/' '-' '+' '_' '@' '^' '$' '*' '?' '<' '>' '=' '&' '{' '}' '(' ')' '[' ']' '|' '\\']
let digits = digit+
let realnumber = '-'? digits ('.' digit*)? ("e" '-' digit digit)?
let comment = "(*" (alpha+|digits|blank+|char)* "*)"


let mult = 'm' digits
let mults = mult? ("_" mult)*

let celerity = "C_" alpha+ "__" mults "__" digits


rule token = parse
  blank              { token lexbuf }     (* skip blanks *)
  | comment          { token lexbuf }
  | ';'              { SEMICOLON }
  | celerity as c    { CEL(c) }
  | '='              { EQ }
  | realnumber as v  { VALUE(v) }
  | eof              { raise Eof }
  | _                { syntax_error "couldn't identify the token" }
