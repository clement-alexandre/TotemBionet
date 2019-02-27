#!/bin/sh

# select path if exists 
# filename=$(basename $1) && \
filename=$1 && \
finalDir=$3 && \
ocamllex -q lexer.mll && \
ocamlyacc parser.mly && \
echo "type durationTerm =
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
	| IsCyclic of bool\n" | cat - parser.mli > temp && mv temp parser.mli && \
ocamlc -c parser.mli && \
ocamlc -c lexer.ml && \
ocamlc -c parser.ml && \
ocamlc -c types.ml && \
ocamlc -c display.ml && \
ocamlc -c parse.ml && \
ocamlc -c extraction_data.ml && \
ocamlc -c standard_functions.ml && \
ocamlc -c dnf_discrete_condition.ml && \
ocamlc -c set_states.ml && \
ocamlc -c strongest_postcondition.ml && \
ocamlc -c wp_properties.ml && \
ocamlc -c simplification_hybrid_condition.ml && \
ocamlc -c valuation.ml && \
ocamlc -c wp_computation.ml && \
ocamlc -c debugging.ml && \
ocamlc -c dnf_constraints.ml && \
ocamlc -c output_files.ml && \
ocamlc -c main.ml && \
ocamlc -o main types.cmo display.cmo lexer.cmo parser.cmo parse.cmo extraction_data.cmo standard_functions.cmo dnf_discrete_condition.cmo set_states.cmo strongest_postcondition.cmo wp_properties.cmo simplification_hybrid_condition.cmo valuation.cmo wp_computation.cmo debugging.cmo dnf_constraints.cmo output_files.cmo main.cmo && \

cd ../ && \
# if [ $# -eq 2 ]
# 	then
		newDir=$(dirname $2)
		if [ -d "$newDir" ]
			then
				./constraints/main $filename $2 $finalDir
			else
				mkdir $newDir
				./constraints/main $filename $2 $finalDir
		fi
# 	else
# 		DATE=`date +%Y-%m-%d`
# 		if [ -d $DATE ]
# 			then
# 				./constraints/main $filename $DATE/$DATE
# 			else
# 				mkdir $DATE && \
# 				./constraints/main $filename $DATE/$DATE
# 		fi
# fi


rm -f ./constraints/*.cmi ./constraints/*.cmo ./constraints/*.mli ./constraints/lexer.ml ./constraints/parser.ml ./constraints/main
