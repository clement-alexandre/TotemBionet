(* File lexer.mll *)
{
  open Parser_ig        (* The type token is defined in parser.mli *)
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
let realnumber = '-'? digits '.' digits
let comment = "(*" (alpha+|digits|blank+|char)* "*)"

(* Influence Graph *)
let mult = 'm' digit+
let formula = ("formula:"|"Formula:"|"FORMULA:")
let target = ("targets:"|"Targets:"|"TARGETS:"|"target:"|"Target:"| "TARGET:")

(* Cyclic behaviour *)
let cyclicBritish = ("Cyclic behaviour" | "Cyclic Behaviour" | "cyclic behaviour" | "CYCLIC BEHAVIOUR")
let cyclicAmerican = ("Cyclic behavior" | "Cyclic Behavior" | "cyclic behavior" | "CYCLIC BEHAVIOR")

let comp = ['>' '<' '=' ] | "!=" | ">=" | "<=" | "=>" | "=<"


(* Hoare Triple *)
let ht = "Start Hoare Triple" (_|char)+ "End Hoare Triple" (_+)?




rule token = parse
  blank                                               { token lexbuf }     (* skip blanks *)
  | comment                                           { token lexbuf }
  | ht                                                { token lexbuf }
  | ','                                               { COMMA }
  | ';'                                               { SEMICOLON }
  | '('                                               { LPAREN }
  | ')'                                               { RPAREN }
        (* Influence Graph *)
  | "Start Influence Graph"                           { STARTIG }
  | "End Influence Graph"                             { ENDIG }
  | ("var"|"Var"|"VAR")                               { VARIABLE }
  | ("mult"|"Mult"|"MULT")                            { MULTIPLEX }
  | formula                                           { FORMULA }
  | target                                            { TARGET }
  | mult as m                                         { MULTVAR(m) }
  | digits as d                                       { INT(d) }
  | comp as c                                         { COMP(c) }
  | ("AND"|"and"|"And")                               { AND }
  | ("OR"|"or"|"Or")                                  { OR }
  | ("IMPL"|"impl"|"Impl")                            { IMPL }
  | ("NEG"|"Neg"|"neg")                               { NEG }
  | alpha+ as a                                       { VAR(a) }
  | eof                                               { raise Eof }
  | _                                                 { syntax_error "couldn't identify the token" }
