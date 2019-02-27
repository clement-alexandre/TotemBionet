(*****************************************)
(*** Grammatical syntax for the parser ***)
(*****************************************)

(* This file defines the grammatical syntax of the input file.
This grammar is recognized by the lexer, and a rule is applied
depending on the grammar *)

{
  open Parser        (* The type token is defined in parser.mli *)
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
let realnumber = '-'? digits '.' digits
let comment = "(*" (alpha+|digits|blank+|char)* "*)"

(* Influence Graph *)
let mult = 'm' digit+
let formula = ("formula:"|"Formula:"|"FORMULA:")
let target = ("targets:"|"Targets:"|"TARGETS:"|"target:"|"Target:"| "TARGET:")

(* Cyclic behaviour *)
let cyclicBritish = ("Cyclic behaviour" | "Cyclic Behaviour" | "cyclic behaviour" | "CYCLIC BEHAVIOUR")
let cyclicAmerican = ("Cyclic behavior" | "Cyclic Behavior" | "cyclic behavior" | "CYCLIC BEHAVIOR")

(* Duration terms *)
let varTime = 't' digit+
let symbol = ['+' '-']

(* Assertion terms *)
let boolean = ("True"|"true"|"TRUE"|"False"|"false"|"FALSE")
let slide = ("slide"|"Slide"|"SLIDE")
let noslide = ("noslide"|"noSlide"|"NOSLIDE"|"Noslide"|"NoSlide"|"NOslide"|"NOSlide")
let comp = ['>' '<' '=' ] | "!=" | ">=" | "<=" | "=>" | "=<"
let cel = ("C("| "c(")

(* Discrete path atom *)
let dpa = alpha+ symbol

(* Language property *)
let eta = ("eta"|"Eta")
let connectives = ['+' '-' '*' '/']
let piexit = ("pi'"|"Pi'"|"PI'")
let pientrance = ("pi"|"Pi"|"PI")
let time = ("time"|"Time"|"TIME")



(* Definition of the grammar observed into the input file *)
rule token = parse
  blank                                               { token lexbuf }     (* skip blanks *)
  | comment                                           { token lexbuf }
  | ','                                               { COMMA }
  | ';'                                               { SEMICOLON }
  | '('                                               { LPAREN }
  | ')'                                               { RPAREN }
  | '{'                                               { LBRACKET }
  | '}'                                               { RBRACKET }
  | '['                                               { LSBRACKET }
  | ']'                                               { RSBRACKET }
  | "Start Hoare Triple"                              { STARTHT }
  | (cyclicBritish|cyclicAmerican)                    { CYCLIC }
  | "End Hoare Triple"                                { ENDHT }
        (* Influence Graph *)
  | "Start Influence Graph"                           { STARTIG }
  | "End Influence Graph"                             { ENDIG }
  | ("var"|"Var"|"VAR")                               { VARIABLE }
  | ("mult"|"Mult"|"MULT")                            { MULTIPLEX }
  | formula                                           { FORMULA }
  | target                                            { TARGET }
  | mult as m                                         { MULTVAR(m) }
        (* Duration *)
  | realnumber as r                                   { REALNUMBER(r) } 
  | digits as d                                       { INT(d) }   
  | varTime as v                                      { VARTIME(v) }
        (* Assertion *)
  | slide as s                                        { SLIDE(s) }
  | noslide as n                                      { NOSLIDE(n) }
  | comp as c                                         { COMP(c) }
  | cel as c                                          { CEL(c) }
  | boolean as r                                      { BOOLEAN(r) }
  | ("AND"|"and"|"And")                               { AND }
  | ("OR"|"or"|"Or")                                  { OR }
  | ("IMPL"|"impl"|"Impl")                            { IMPL }
  | ("NEG"|"Neg"|"neg")                               { NEG }
        (* Discrete path atom *)
  | symbol as s                                       { SYMBOL(s) }
        (* Postcondition : Property Language (D,H) *)
  | eta as e                                          { ETA(e) }
  | connectives as c                                  { CONNECTIVES(c) }
  | piexit as p                                       { PIEXIT(p)}
  | pientrance as p                                   { PIENTRANCE(p) }
  | time as t                                         { TIME(t) }
        (* Definition of variable *)
  | alpha+ as a                                       { VAR(a) }
  | eof                                               { raise Eof }
  | _                                                 { syntax_error "couldn't identify the token" }
