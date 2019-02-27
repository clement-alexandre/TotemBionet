(********************************)
(*** Parser of the input file ***)
(********************************)

(* This program reads the input file containing celerities.
We select one set for the simulator *)

open Types_parameters;;
open Printf;;


(*******************************)
(*** Identification of types ***)
(*******************************)

(* Definition of the celerities *)
let eval_parameters p =
	let calc min max =
		string_of_float((float_of_string(min) +. float_of_string(max)) /. 2.)
	in
	let rec compute_value l =
		match l with
			| h::[] -> h
			| h::t -> string_of_float(float_of_string(h ^ ".0") /. float_of_string((compute_value t) ^ ".0"))
			| other -> raise(Invalid_argument "compute_value: error")
	in
	let identify_value v =
		match v with
			| Parser_parameters.Int(i) -> i ^ ".0"
			| Parser_parameters.RealNumber(r) -> r
			| Parser_parameters.Rational(r) -> compute_value (Str.split (Str.regexp "/") r)
	in
	let identify_cel p =
		match p with
			| Parser_parameters.CelVal(cel, min, max) -> cel ^ "=" ^ calc (identify_value min) (identify_value max) ^ ";"
	in identify_cel p
;;



(************************************)
(*** Main functions of the parser ***)
(************************************)


(* Read the file with the lexer and parser, and create an output file
containing the celerities with their values *)
let input_parameters = ref "";;
let rec parse oe =
	let lexbuf = Lexing.from_channel (oe) in
	try
		while true do
			let token = Parser_parameters.program Lexer_parameters.token lexbuf in
			print_endline(eval_parameters token);
			(* input_parameters :=  (eval_parameters token) ^ (!input_parameters); *)
		done		
	with Lexer_parameters.Eof ->
		printf("")
;;

(* Main function which parse the input file *)
let _ =
	let filename = Sys.argv.(1) in
	(* printf "Input file name : %s\n" filename; *)
	let oe = open_in filename in
	try 
		parse oe;
		(* print_endline(!input_parameters); *)
	with End_of_file -> ()
;;



