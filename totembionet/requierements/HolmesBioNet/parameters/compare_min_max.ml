let extract_data elt =
	let select_value e =
		match e with
			| h::[] -> h
			| other -> raise(Invalid_argument "extract_data: error")
		in
	let select_elt e =
		match e with
			| h::t -> (h, select_value t)
			| other -> raise(Invalid_argument "extract_data: error")
	in select_elt (Str.split (Str.regexp ":") elt)
;;

let extract_val elt =
	let select_min e =
		match e with
			| [] -> raise(Invalid_argument "extract_val : error")
			| h::t -> h
	in
	let rec select_max e =
		match e with 
			| [] -> raise(Invalid_argument "extract_val : error")
			| h::[] -> h
			| h::t -> select_max t
	in (select_min elt, select_max elt)
;;

let rec read_file f =
	match input_line f with
		| line -> 
			let (cel, value) = extract_data line in
			let (min, max) = extract_val (Str.split (Str.regexp ";") value) in
			(* Random.float n gives a float number between 0 (inclusive) and n (inclusive)
			So we add 2*precision in order to exclude min and max *)
			if ( float_of_string(min) != float_of_string(max))
			then (
				let prec = 1e-12 in
				let randomValue = Random.float (float_of_string(max) -. float_of_string(min) -. (2. *. prec)) in
				let res = randomValue +. float_of_string(min) +. prec in
				print_endline("\t" ^ cel ^ "=" ^ string_of_float(res) ^ ";")
			)
			else (
				print_endline("\t" ^ cel ^ "=" ^ min ^ ";")
				)
		| exception End_of_file -> close_in f
;;



(* Main function *)
let _ =
	Random.self_init();
	let file = Sys.argv.(1) in
	let ic = open_in file in
	try
		read_file ic;		
	with
	| _ -> failwith "Can not read the input file"