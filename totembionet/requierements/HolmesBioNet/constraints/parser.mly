%{
type durationTerm =
	ValueTime of string
	| VarTime of string

type dpaTerm =
	DpaValue of string * char

type discreteTerm =
	Eta of string
	| DConstant of string

type discreteAtom = 
	DTermUn of discreteTerm
	| DTermRelation of discreteAtom * char * discreteAtom

type discreteCondition =
	DBoolean of string
	| DAtom of discreteAtom * string * discreteAtom
	| DOpAnd of discreteCondition * discreteCondition
	| DOpOr of discreteCondition * discreteCondition
	| DNeg of discreteCondition
	| DPar of discreteCondition

type resource =
	Multiplex of string
	| SetMultiplexes of string * resource

type hybridTerm =
	PiEntrance of string
	| PiExit of string
	| Celerity of string * resource * string
	| CelerityNR of string * string
	| HTermRelation of hybridTerm * char * hybridTerm
	| HConstant of string
	| HTime of durationTerm

type hybridCondition =
	HBoolean of string
	| HAtom of hybridTerm * string * hybridTerm
	| HDAtom of discreteAtom * string * discreteAtom
	| HOpAnd of hybridCondition * hybridCondition
	| HOpOr of hybridCondition * hybridCondition
	| HNeg of hybridCondition
	| HPar of hybridCondition

type assertionTerm =
	ASlide of string
	| ANoSlide of string
	| ASlideSymbol of char * string
	| ANoSlideSymbol of char * string
	| ACelerity of string * string * hybridTerm
	| ABoolean of string
	| AOpAnd of assertionTerm * assertionTerm
	| AOpOr of assertionTerm * assertionTerm
	| ANeg of assertionTerm
	| APar of assertionTerm

type elementaryPath =
	Tuple of durationTerm * assertionTerm * dpaTerm

type path = 
	EPath of elementaryPath
	| Seq of elementaryPath * path

type postcondition =
	PostCondition of discreteCondition * hybridCondition

type multformula =
	MAtom of string * string * string
	| MPropNeg of multformula
	| MPropAnd of multformula * multformula
	| MPropOr of multformula * multformula
	| MPropImpl of multformula * multformula
	| MPropPar of multformula

type varlist =
	MVar of string
	| MVarList of string * varlist

type target =
	MTarget of varlist

type formula =
	MFormula of multformula

type predecessor = 
	VarDefinition of string * string
	| MultDefinition of string * formula * target

type influenceGraph =
	PredecessorDef of predecessor
	| SeveralVarDef of predecessor * influenceGraph

type fileLine = 
	HoareTriple of path * postcondition
	| InfluenceGraph of influenceGraph
	| IsCyclic of bool


%}

/* File parser.mly */
%token COMMA 
%token SEMICOLON
%token LPAREN RPAREN LBRACKET RBRACKET LSBRACKET RSBRACKET
%token STARTHT ENDHT CYCLIC STARTIG ENDIG VARIABLE MULTIPLEX FORMULA TARGET
%token <string> MULTVAR INT REALNUMBER VARTIME SLIDE NOSLIDE CEL BOOLEAN DPA VAR COMP ETA PIENTRANCE PIEXIT TIME 
%token <char> SYMBOL CONNECTIVES
%token AND OR IMPL NEG
%token EOL
%token EOF

%nonassoc INT  /* lowest precedence */
%left SEMICOLON
%left SYMBOL
%left CONNECTIVES
%left AND OR IMPL NEG
%left LPAREN RPAREN
        
%start program                                     /* the entry point */
%type <fileLine> program

%%
program:
	STARTHT LBRACKET RBRACKET path postcondition ENDHT              { HoareTriple($4,$5) }
	| CYCLIC                                                        { IsCyclic(true) }
	| STARTIG influenceGraph ENDIG                                  { InfluenceGraph($2) }

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

path:
	elementaryPath                                                  { EPath($1) }
	| elementaryPath SEMICOLON path                                 { Seq($1,$3) }
;

elementaryPath:
	LPAREN tuple RPAREN                                             { $2 }	
;

tuple:
	duration COMMA assertion COMMA dpa                              { Tuple($1,$3,$5) }
;


duration:
	INT                                                             { ValueTime($1) }
	| REALNUMBER                                                    { ValueTime($1) }
	| VARTIME                                                       { VarTime($1) }
;

assertion:
	BOOLEAN                                                         { ABoolean($1) }
	| CEL VAR RPAREN COMP hybridTerm                                { ACelerity($2,$4,$5) }
	| SLIDE SYMBOL LPAREN VAR RPAREN                                { ASlideSymbol($2,$4) }
	| NOSLIDE SYMBOL LPAREN VAR RPAREN                              { ANoSlideSymbol($2,$4) }
	| SLIDE LPAREN VAR RPAREN                                       { ASlide($3) }
	| NOSLIDE LPAREN VAR RPAREN                                     { ANoSlide($3) }
	| NEG LPAREN assertion RPAREN                                   { ANeg($3) }
	| assertion AND assertion                                       { AOpAnd($1,$3) }
	| assertion OR assertion                                        { AOpOr($1,$3) }
	| LPAREN assertion RPAREN                                       { APar($2) }
;

dpa:
	VAR SYMBOL                                                      { DpaValue($1,$2) }
;

postcondition:
	LBRACKET discreteCondition COMMA hybridCondition RBRACKET       { PostCondition($2,$4) }
;

hybridCondition:
	BOOLEAN                                                         { HBoolean($1) }
	| hybridTerm COMP hybridTerm                                    { HAtom($1,$2,$3) }
	| discreteAtom COMP discreteAtom                                { HDAtom($1,$2,$3) }
	| NEG LPAREN hybridCondition RPAREN                             { HNeg($3) }
	| hybridCondition AND hybridCondition                           { HOpAnd($1,$3) }
	| hybridCondition OR hybridCondition                            { HOpOr($1,$3) }
	| LPAREN hybridCondition RPAREN                                 { HPar($2) }
;


hybridTerm:
	PIENTRANCE LPAREN VAR RPAREN                                    { PiEntrance($3) }
	| PIEXIT LPAREN VAR RPAREN                                      { PiExit($3) }
	| CEL VAR COMMA LSBRACKET resources RSBRACKET COMMA INT RPAREN  { Celerity($2,$5,$8) }
	| CEL VAR COMMA LSBRACKET RSBRACKET COMMA INT RPAREN            { CelerityNR($2,$7) }
	| REALNUMBER                                                    { HConstant($1) }
	| hybridTerm CONNECTIVES hybridTerm                             { HTermRelation($1,$2,$3) }
	| TIME LPAREN duration RPAREN                                   { HTime($3) }
;

resources:
	MULTVAR                                                         { Multiplex($1) }
	| MULTVAR COMMA resources                                       { SetMultiplexes($1,$3) }
;

discreteCondition:
	BOOLEAN                                                         { DBoolean($1) }
	| discreteAtom COMP discreteAtom                                { DAtom($1,$2,$3) }
	| NEG LPAREN discreteCondition RPAREN                           { DNeg($3) }
	| discreteCondition AND discreteCondition                       { DOpAnd($1,$3) }
	| discreteCondition OR discreteCondition                        { DOpOr($1,$3) }
	| LPAREN discreteCondition RPAREN                               { DPar($2) }
;

discreteAtom:
	discreteAtom SYMBOL discreteAtom                                { DTermRelation($1,$2,$3) }
	| discreteTerm                                                  { DTermUn($1) }
;

discreteTerm:
	ETA LPAREN VAR RPAREN                                           { Eta($3) }
	| INT                                                           { DConstant($1) }
;



%%




