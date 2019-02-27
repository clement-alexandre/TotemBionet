(* This program reads input files containing hybrid Hoare logic *)
(* Starting from this input file, a parser is used in order to create 
the *)

open Types;;
open Printf;;


(* Definition of the different types given by the parser *)
let eval_cel c =
	let rec identify_cel c =
		match c with
			| Parser_cel.IdentifyCelValue(cel,value) -> (cel,float_of_string(value))
	in identify_cel c
;;

(* TODO : TEMPORARY PRINT *)

let rec print_celList cl =
	match cl with
		| [] -> ""
		| (c,v)::[] -> "(" ^ c ^ ", " ^ (string_of_float v) ^ ")"
		| (c,v)::t -> "(" ^ c ^ ", " ^ (string_of_float v) ^ "); " ^ print_celList t
;;


(* Function which does the parsing of the influence graph.
The global extraction_data variable contains information
about the influence graph, the variables and the 
multiplexes defined in the influence graph. *)
let extraction_cel = ref [];;
let rec parse oe =
	let lexbuf = Lexing.from_channel (oe) in
	try
		while true do
			let token = Parser_cel.program Lexer_cel.token lexbuf in
			extraction_cel :=  (eval_cel token)::(!extraction_cel);
		done		
	with Lexer_cel.Eof ->
		printf("")
;;


(* Main function for parsing the input file and define a new
syntax for the influence graph, mandatory for the simulator *)
let parse_cel =
	let filename = Sys.argv.(2) in
	(* printf "Input file name : %s\n" filename; *)
	let oe = open_in filename in
	try 
		parse oe;
	with End_of_file -> ()
;;



