#!/bin/sh

filename=$(basename $1) &&\

# Create dinit and piinit file
# grep -E -o '.*(eta|Eta).*' $1 | head -1 > temp_dinit && \
# grep -E -o '.*(pi|Pi|PI).*' $1 | head -1 > piinit && \

# sed -ie 's/ And /\n/g' temp_dinit   && \
# sed -ie 's/ and /\n/g' temp_dinit   && \
# sed -ie 's/ AND /\n/g' temp_dinit   && \
# sed -ie 's/,//g' temp_dinit   && \
# sed -ie 's/ And /\n/g' piinit   && \
# sed -ie 's/ and /\n/g' piinit   && \
# sed -ie 's/ AND /\n/g' piinit   && \




# Parser for the influence graph
ocamllex -q lexer_ig.mll && \
ocamlyacc parser_ig.mly && \
echo "type multformula =
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
	InfluenceGraph of influenceGraph\n" | cat - parser_ig.mli > temp && mv temp parser_ig.mli && \
ocamlc -c parser_ig.mli && \
ocamlc -c lexer_ig.ml && \
ocamlc -c parser_ig.ml && \


# Parser for the celerities
ocamllex -q lexer_cel.mll && \
ocamlyacc parser_cel.mly && \
echo "type fileLine = 
	IdentifyCelValue of string * string" | cat - parser_cel.mli > temp && mv temp parser_cel.mli && \
ocamlc -c parser_cel.mli && \
ocamlc -c lexer_cel.ml && \
ocamlc -c parser_cel.ml && \


# Parser for the initial discrete and hybrid conditions
ocamllex -q lexer_dinit.mll && \
ocamlyacc parser_dinit.mly && \
echo "type dcondition =
	ValueEta of string * string
	| RemovePar of dcondition

type dinitLine =
	DCondition of dcondition\n" | cat - parser_dinit.mli > temp && mv temp parser_dinit.mli && \
ocamlc -c parser_dinit.mli && \
ocamlc -c lexer_dinit.ml && \
ocamlc -c parser_dinit.ml && \


ocamllex -q lexer_piinit.mll && \
ocamlyacc parser_piinit.mly && \
echo "type hcondition =
	ValuePi of string * string
	| RemovePar of hcondition

type piinitLine =
	HCondition of hcondition\n" | cat - parser_piinit.mli > temp && mv temp parser_piinit.mli && \
ocamlc -c parser_piinit.mli && \
ocamlc -c lexer_piinit.ml && \
ocamlc -c parser_piinit.ml && \

# Simulation with all entrance files
ocamlc -c types.ml && \
ocamlc -c display.ml && \
ocamlc -c parse_ig.ml && \
ocamlc -c parse_cel.ml && \
ocamlc -c parse_dinit.ml && \
ocamlc -c parse_piinit.ml && \
ocamlc -c simulation.ml && \
ocamlc -c output_file.ml && \
ocamlc -c main.ml && \
ocamlc -o simulate lexer_ig.cmo parser_ig.cmo lexer_cel.cmo parser_cel.cmo lexer_dinit.cmo parser_dinit.cmo lexer_piinit.cmo parser_piinit.cmo types.cmo display.cmo parse_ig.cmo parse_cel.cmo simulation.cmo output_file.cmo parse_dinit.cmo parse_piinit.cmo main.cmo && \
./simulate $1 $2 $3/dinit $3/piinit $4 $3/graph_data.txt && \

rm -f temp* *.cmi *.cmo *.mli lexer_ig.ml parser_ig.ml lexer_cel.ml parser_cel.ml simulate
