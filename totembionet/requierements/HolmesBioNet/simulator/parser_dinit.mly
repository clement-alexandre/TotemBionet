%{

type dcondition =
	ValueEta of string * string
	| RemovePar of dcondition

type dinitLine =
	DCondition of dcondition

%}


/* File parser.mly */
%token COMMA
%token LPAREN RPAREN
%token ETA EQ
%token <string> VAR INT
%token EOL
%token EOF

%nonassoc INT  /* lowest precedence */
%left LPAREN RPAREN
        
%start program                                     /* the entry point */
%type <dinitLine> program

%%

program:
	dcondition { DCondition($1) }
;

dcondition:
	ETA LPAREN VAR RPAREN EQ INT { ValueEta($3,$6) }
	| LPAREN dcondition RPAREN { RemovePar($2) }
;

%%




