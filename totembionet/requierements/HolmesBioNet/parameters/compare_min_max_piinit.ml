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

let rec read_pi_value l =
	let (pi, value) = extract_data l in
	let (min, max) = extract_val (Str.split (Str.regexp ";") value) in
	(* Random.float n gives a float number between 0 (inclusive) and n (inclusive)
	So we add 2*precision in order to exclude min and max *)
	if ( float_of_string(min) != float_of_string(max))
	then (
		let prec = 1e-12 in
		let randomValue = Random.float (float_of_string(max) -. float_of_string(min) -. (2. *. prec)) in
		let res = randomValue +. float_of_string(min) +. prec in
		print_endline("\t" ^ pi ^ "=" ^ string_of_float(res))
	)
	else (
		print_endline("\t" ^ pi ^ "=" ^ min)
	)
;;



(* Main function *)
let _ =
	let pi_value = Sys.argv.(1) in
	try
		Random.self_init();
		read_pi_value pi_value;	
	with
	| _ -> failwith "Can not read the input file"