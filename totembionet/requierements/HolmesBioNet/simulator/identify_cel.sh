#!/bin/sh

directory=$(dirname $1) && \
# echo $directory

ocamllex -q lexer_parameters.mll && \
ocamlyacc parser_parameters.mly && \
echo "type floatValue =
	| RealNumber of string
	| Rational of string
	| Int of string

type fileLine = 
	CelVal of string * floatValue * floatValue" | cat - parser_parameters.mli > temp && mv temp parser_parameters.mli && \
ocamlc -c parser_parameters.mli && \
ocamlc -c lexer_parameters.ml && \
ocamlc -c parser_parameters.ml && \
ocamlc -c types_parameters.ml && \
ocamlc -c parse_parameters.ml && \
ocamlc -o main_parameters str.cma types_parameters.cmo lexer_parameters.cmo parser_parameters.cmo parse_parameters.cmo && \

./main_parameters $1 > $directory/data_cel.txt && \


rm -f temp *.cmi *.cmo *.mli lexer_parameters.ml parser_parameters.ml main_parameters

