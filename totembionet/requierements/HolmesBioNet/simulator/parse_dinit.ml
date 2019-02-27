(* This program reads input files containing hybrid Hoare logic *)
(* Starting from this input file, a parser is used in order to create 
the *)

open Types;;
open Printf;;


(* Definition of the different types given by the parser *)
let eval_dinit d =
	let rec eval_d d =
		match d with
			| Parser_dinit.ValueEta(v,i) -> (v, int_of_string i)
			| Parser_dinit.RemovePar(dc) -> eval_d dc
		in
	let rec identify_d d =
		match d with
			| Parser_dinit.DCondition(dc) -> eval_d dc
	in identify_d d
;;

(* Function which does the parsing of the initial discrete condition.
The global extraction_dinit variable contains information
about the initial discrete condition *)
let extraction_dinit = ref [];;
let rec parse oe =
	let lexbuf = Lexing.from_channel (oe) in
	try
		while true do
			let token = Parser_dinit.program Lexer_dinit.token lexbuf in
			extraction_dinit :=  (eval_dinit token)::(!extraction_dinit);
		done		
	with Lexer_dinit.Eof ->
		printf("")
;;


(* Main function for parsing the input file and define a new
syntax for the influence graph, mandatory for the simulator *)
let parse_dinit =
	let filename = Sys.argv.(3) in
	(* printf "Input file name : %s\n" filename; *)
	let oe = open_in filename in
	try 
		parse oe;
	with End_of_file -> ()
;;



