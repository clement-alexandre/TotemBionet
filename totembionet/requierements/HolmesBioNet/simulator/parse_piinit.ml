(* This program reads input files containing hybrid Hoare logic *)
(* Starting from this input file, a parser is used in order to create 
the *)

open Types;;
open Printf;;


(* Definition of the different types given by the parser *)
let eval_piinit pi =
	let rec eval_pi d =
		match d with
			| Parser_piinit.ValuePi(pi,i) -> (pi, float_of_string i)
			| Parser_piinit.RemovePar(hc) -> eval_pi hc
		in
	let rec identify_pi pi =
		match pi with
			| Parser_piinit.HCondition(hc) -> eval_pi hc
	in identify_pi pi
;;

(* Function which does the parsing of the initial discrete condition.
The global extraction_piinit variable contains information
about the initial discrete condition *)
let extraction_piinit = ref [];;
let rec parse oe =
	let lexbuf = Lexing.from_channel (oe) in
	try
		while true do
			let token = Parser_piinit.program Lexer_piinit.token lexbuf in
			extraction_piinit :=  (eval_piinit token)::(!extraction_piinit);
		done		
	with Lexer_piinit.Eof ->
		printf("")
;;


(* Main function for parsing the input file and define a new
syntax for the influence graph, mandatory for the simulator *)
let parse_piinit =
	let filename = Sys.argv.(4) in
	(* printf "Input file name : %s\n" filename; *)
	let oe = open_in filename in
	try 
		parse oe;
	with End_of_file -> ()
;;



