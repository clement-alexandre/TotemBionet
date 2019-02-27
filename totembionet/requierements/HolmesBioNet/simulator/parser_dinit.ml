type token =
  | COMMA
  | LPAREN
  | RPAREN
  | ETA
  | EQ
  | VAR of (string)
  | INT of (string)
  | EOL
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parser_dinit.mly"

type dcondition =
	ValueEta of string * string
	| RemovePar of dcondition

type dinitLine =
	DCondition of dcondition

# 24 "parser_dinit.ml"
let yytransl_const = [|
  257 (* COMMA *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* ETA *);
  261 (* EQ *);
  264 (* EOL *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  262 (* VAR *);
  263 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\000\000"

let yylen = "\002\000\
\001\000\006\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\004\000\001\000\000\000\000\000\
\003\000\000\000\000\000\000\000\002\000"

let yydgoto = "\002\000\
\005\000\006\000"

let yysindex = "\002\000\
\254\254\000\000\254\254\255\254\000\000\000\000\001\255\000\255\
\000\000\002\255\003\255\004\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\004\000"

let yytablesize = 11
let yytable = "\003\000\
\008\000\004\000\001\000\009\000\011\000\010\000\007\000\012\000\
\000\000\000\000\013\000"

let yycheck = "\002\001\
\002\001\004\001\001\000\003\001\003\001\006\001\003\000\005\001\
\255\255\255\255\007\001"

let yynames_const = "\
  COMMA\000\
  LPAREN\000\
  RPAREN\000\
  ETA\000\
  EQ\000\
  EOL\000\
  EOF\000\
  "

let yynames_block = "\
  VAR\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'dcondition) in
    Obj.repr(
# 30 "parser_dinit.mly"
            ( DCondition(_1) )
# 95 "parser_dinit.ml"
               : dinitLine))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 34 "parser_dinit.mly"
                              ( ValueEta(_3,_6) )
# 103 "parser_dinit.ml"
               : 'dcondition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'dcondition) in
    Obj.repr(
# 35 "parser_dinit.mly"
                            ( RemovePar(_2) )
# 110 "parser_dinit.ml"
               : 'dcondition))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : dinitLine)
;;
# 39 "parser_dinit.mly"




# 140 "parser_dinit.ml"
