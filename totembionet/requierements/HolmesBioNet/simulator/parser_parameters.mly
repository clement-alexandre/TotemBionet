%{

type floatValue =
	| RealNumber of string
	| Rational of string
	| Int of string


type fileLine = 
	CelVal of string * floatValue * floatValue


%}

/* File parser.mly */
%token SEMICOLON EQ COMMA
%token LSBRACKET RSBRACKET
%token <string> CEL REALNUMBER RATIONAL INT
%token EOL
%token EOF

/* lowest precedence */
%left SEMICOLON EQ
%left LSBRACKET RSBRACKET
        
%start program                                     /* the entry point */
%type <fileLine> program

%%
program:
	CEL EQ LSBRACKET value SEMICOLON value RSBRACKET  { CelVal($1,$4,$6) }
;

value:
	REALNUMBER   { RealNumber($1) }
	| RATIONAL   { Rational($1) }
	| INT        { Int($1) }

%%




