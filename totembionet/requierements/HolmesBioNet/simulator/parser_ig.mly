%{

type multformula =
	MAtom of string * string * string
	| MPropNeg of multformula
	| MPropAnd of multformula * multformula
	| MPropOr of multformula * multformula
	| MPropImpl of multformula * multformula
	| MPropPar of multformula

type formula =
	MFormula of multformula

type varlist =
	MVar of string
	| MVarList of string * varlist

type target =
	MTarget of varlist

type predecessor = 
	VarDefinition of string * string
	| MultDefinition of string * formula * target

type influenceGraph =
	PredecessorDef of predecessor
	| SeveralVarDef of predecessor * influenceGraph

type fileLine = 
	InfluenceGraph of influenceGraph


%}

/* File parser.mly */
%token COMMA 
%token SEMICOLON
%token LPAREN RPAREN
%token STARTHT ENDHT STARTIG ENDIG VARIABLE MULTIPLEX FORMULA TARGET
%token <string> MULTVAR INT VAR COMP 
%token <char> CONNECTIVES
%token AND OR IMPL NEG
%token EOL
%token EOF

%nonassoc INT  /* lowest precedence */
%left SEMICOLON
%left CONNECTIVES
%left AND OR IMPL NEG
%left LPAREN RPAREN
        
%start program                                     /* the entry point */
%type <fileLine> program

%%
program:
	STARTIG influenceGraph ENDIG                                  { InfluenceGraph($2) }

;

influenceGraph:
	predecessor SEMICOLON                                           { PredecessorDef($1) }
	| predecessor SEMICOLON influenceGraph                          { SeveralVarDef($1,$3) }
;

predecessor:
	VARIABLE VAR INT                                                { VarDefinition($2,$3) }
	| MULTIPLEX MULTVAR formula target                              { MultDefinition($2,$3,$4) }
;

formula:
	FORMULA multformula                                             { MFormula($2) }
;

target:
	TARGET varlist                                                  { MTarget($2) }
;

varlist:
	VAR                                                             { MVar($1) }
	| VAR COMMA varlist                                             { MVarList($1,$3) }


multformula:
	VAR COMP INT                                                    { MAtom($1,$2,$3) }
	| NEG LPAREN multformula RPAREN                                 { MPropNeg($3) }
	| multformula AND multformula                                   { MPropAnd($1,$3) }
	| multformula OR multformula                                    { MPropOr($1,$3) }
	| multformula IMPL multformula                                  { MPropImpl($1,$3) }
	| LPAREN multformula RPAREN                                     { MPropPar($2) }
;



%%




