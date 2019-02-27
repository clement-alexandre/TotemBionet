open Simulation;;
open Printf;;



(*******************************************
  Part 5 :
  save to file ...
 *******************************************)
let rec additionne(d,pi,vars)=
  match vars with
    [] -> []
  | v1::ll -> (v1,float(valueOfAt(v1,d))+.valueOfAt(v1,pi))::additionne(d,pi,ll);;


(* TODO: temporary display for debugging *)
let print_trace trace vars =
(*   let rec eval_d_pi d pi =
    match d with
      | [] -> "??"
      | (v,dv)::[] -> v ^ ":" ^ string_of_float((float_of_int dv) +. (List.assoc v pi))
      | (v,dv)::t -> v ^ ":" ^ string_of_float((float_of_int dv) +. (List.assoc v pi))
      (* | (v,dv)::t -> v ^ ":" ^ string_of_float((float_of_int dv) +. (List.assoc v pi)) ^ "\t" ^ (eval_d_pi t pi) *)
    in *)
  let rec eval_v v =
    match v with
     | [] -> "??"
     | (v,value)::[] -> string_of_float (value)
     (* | (v,value)::[] -> v ^ ":" ^ string_of_float (value) *)
     | (v,value)::tail -> string_of_float (value) ^ "\t" ^ (eval_v tail)
     (* | (v,value)::tail -> v ^ ":" ^ string_of_float (value) ^ "\t" ^ (eval_v tail) *)
  in
  let rec eval_all_traces t vars = 
    match t with
      | [] -> "No trace"
      (* | (time, (d, pi))::[] -> string_of_float time ^ "\t" ^ (eval_d_pi d pi) *)
      | (time, (d, pi))::[] -> string_of_float time ^ "\t" ^ (eval_v (additionne(d, pi, vars))) 
      (* | (time, (d, pi))::tail -> string_of_float time ^ "\t" ^ (eval_d_pi d pi) ^ "\n" ^ eval_all_traces tail vars *)
      | (time, (d, pi))::tail -> string_of_float time ^ "\t" ^ (eval_v (additionne(d, pi, vars))) ^ "\n" ^ eval_all_traces tail vars
  in eval_all_traces trace vars ;;


let print_trace2 trace =
  let rec eval_pi d =
    match d with
      | [] -> ""
      | (v,piv)::[] -> "(" ^ v ^ "," ^ string_of_float piv ^ ")"
      | (v,piv)::t -> "(" ^ v ^ "," ^ string_of_float piv ^ "); " ^ eval_pi t
    in
  let rec eval_d d =
    match d with
      | [] -> ""
      | (v,dv)::[] -> "(" ^ v ^ "," ^ string_of_int dv ^ ")"
      | (v,dv)::t -> "(" ^ v ^ "," ^ string_of_int dv ^ "); " ^ eval_d t
    in
  let rec eval_all_traces t =
    match t with
      | [] -> "No trace"
      | (time, (d, pi))::[]  -> "Time = " ^ string_of_float time ^ ", [" ^ eval_d d ^ "], [" ^ eval_pi pi ^ "]"
      | (time, (d, pi))::tail  -> "Time = " ^ string_of_float time ^ ", [" ^ eval_d d ^ "], [" ^ eval_pi pi ^ "]\n" ^ eval_all_traces tail
    in eval_all_traces trace;;

let rec print_eta e =
  match e with
    | [] -> "Nothing in eta"
    | (v,i)::[] -> "(\"" ^ v ^ "\"," ^ string_of_int(i) ^ ")"
    | (v,i)::t -> "(\"" ^ v ^ "\"," ^ string_of_int(i) ^ "); " ^ print_eta t
;;

let rec print_pi e =
  match e with
    | [] -> "Nothing in pi"
    | (v,f)::[] -> "(\"" ^ v ^ "\"," ^ string_of_float(f) ^ ")"
    | (v,f)::t -> "(\"" ^ v ^ "\"," ^ string_of_float(f) ^ "); " ^ print_pi t
;;



(* Main function which creates the output file.
Each line represents the exact position 
of each variable at a given time. *)
let create_file trace filename vars =
	let write_legend oc vars =
		let rec write_variables vars =
			match vars with
				| [] -> ""
				| h::[] -> h
				| h::t -> h ^ "\t" ^ (write_variables t)
		in fprintf oc "Time\t%s\n" (write_variables vars)
	in
	let write oc trace vars =
		let rec additionne(d,pi,vars)=
			match vars with
				| [] -> []
				| v1::ll -> (v1,float(valueOfAt(v1,d))+.valueOfAt(v1,pi))::additionne(d,pi,ll)
		in
		let rec eval_v v =
			match v with
				| [] -> ""
        | (v,value)::[] -> string_of_float (value)
				(* | (v,value)::[] -> v ^ ":" ^ string_of_float (value) *)
        | (v,value)::tail -> string_of_float (value) ^ "\t" ^ (eval_v tail)
				(* | (v,value)::tail -> v ^ ":" ^ string_of_float (value) ^ "\t" ^ (eval_v tail) *)
			in
		let rec write_trace t vars =
			match t with
				| [] -> "No trace observed"
				| (time, (d, pi))::[] -> string_of_float time ^ "\t" ^ (eval_v (additionne(d, pi, vars))) ^ "\n"
				| (time, (d, pi))::tail -> string_of_float time ^ "\t" ^ (eval_v (additionne(d, pi, vars))) ^ "\n" ^ write_trace tail vars
		in fprintf oc "%s" (write_trace trace vars)
	in
	let oc = open_out filename in
	write_legend oc vars;
  (* print_endline(string_of_int (List.length trace)); *)
	write oc trace vars;
	close_out oc;;
