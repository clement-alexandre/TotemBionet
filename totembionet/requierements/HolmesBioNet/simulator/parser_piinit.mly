%{

type hcondition =
	ValuePi of string * string
	| RemovePar of hcondition

type piinitLine =
	HCondition of hcondition

%}


/* File parser.mly */
%token COMMA
%token LPAREN RPAREN
%token PI EQ
%token <string> VAR FLOAT
%token EOL
%token EOF

%nonassoc FLOAT  /* lowest precedence */
%left LPAREN RPAREN
        
%start program                                     /* the entry point */
%type <piinitLine> program

%%

program:
	hcondition { HCondition($1) }
;

hcondition:
	PI LPAREN VAR RPAREN EQ FLOAT { ValuePi($3,$6) }
	| LPAREN hcondition RPAREN { RemovePar($2) }
;

%%




