%{

type fileLine = 
	IdentifyCelValue of string * string

%}

/* File parser.mly */
%token SEMICOLON
%token <string> CEL VALUE
%token EQ
%token EOL
%token EOF

%nonassoc INT  /* lowest precedence */
%left SEMICOLON
        
%start program                                     /* the entry point */
%type <fileLine> program

%%
program:
	CEL EQ VALUE SEMICOLON          { IdentifyCelValue($1,$3) }
;

%%




