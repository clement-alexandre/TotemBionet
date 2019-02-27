(* This program reads input files containing hybrid Hoare logic *)
(* Starting from this input file, a parser is used in order to create 
the *)

open Types;;
open Display;;
open Printf;;

(* Definition of the different types given by the parser *)
let eval_ig e =
	let rec eval_multformula m =
		match m with
			| Parser_ig.MAtom(v,comp,i) -> (match comp with
						| "=" -> MPropBin(And,MAtom(v,int_of_string(i)),MPropUn(Neg,MAtom(v,int_of_string(i)+1)))
						| "!=" -> MPropUn(Neg,MPropBin(And,MAtom(v,int_of_string(i)),MPropUn(Neg,MAtom(v,int_of_string(i)+1))))
						| ">" -> MAtom(v,int_of_string(i)+1)
						| ">="|"=>" -> MAtom(v,int_of_string(i))
						| "<" -> MPropUn(Neg,MAtom(v,int_of_string(i)))
						| "<="|"=<" -> MPropUn(Neg,MAtom(v,int_of_string(i)+1))
						| _ -> raise(Invalid_argument "eval_Mcomp : not a comparator !"))
			| Parser_ig.MPropNeg(m1) -> MPropUn(Neg,eval_multformula(m1)) 
			| Parser_ig.MPropAnd(m1,m2) -> MPropBin(And,eval_multformula(m1),eval_multformula(m2))
			| Parser_ig.MPropOr(m1,m2) -> MPropBin(Or,eval_multformula(m1),eval_multformula(m2))
			| Parser_ig.MPropImpl(m1,m2) -> MPropBin(Or,MPropUn(Neg,eval_multformula(m1)),eval_multformula(m2))
			| Parser_ig.MPropPar(m1) -> eval_multformula(m1)
		in
	let eval_formula f =
		match f with
			| Parser_ig.MFormula(f1) -> eval_multformula(f1)
		in
	let rec eval_varlist v =
		match v with
			| Parser_ig.MVar(v1) -> [v1]
			| Parser_ig.MVarList(v1,v2) -> [v1] @ eval_varlist(v2)
		in
	let rec var_as_target v lt =
	match lt with
		| [] -> raise(Invalid_argument "var_as_target: none target for each multiplex!")
		| h::[] -> if (v=h) then true else false
		| h::t -> if (v=h) then true else (var_as_target v t)
		in
	let rec create_list_target v m t =
		match v with
			| [] -> raise(Invalid_argument "create_list_target: none variable defined!")
			| (var,(n,l))::[] -> if (var_as_target var t) then [(var,(n, l @ [m]))] else [(var,(n, l))]
			| (var,(n,l))::tail -> if (var_as_target var t) then [(var,(n, l @ [m]))] @ create_list_target tail m t else [(var,(n, l))] @ create_list_target tail m t
		in
	let eval_target t =
		match t with
			| Parser_ig.MTarget(t1) -> eval_varlist(t1)
		in
	let eval_predecessor p lv lm =
		match p with
			| Parser_ig.VarDefinition(v,n) -> (lv @ [(v,(n,[]))] , lm)
			| Parser_ig.MultDefinition(m,f,t) ->  (create_list_target lv m (eval_target t), lm @ [(m,eval_formula(f))])
		in
	let rec eval_influenceGraph s lv lm =
		match s with
			| Parser_ig.PredecessorDef(p) -> eval_predecessor p lv lm
			| Parser_ig.SeveralVarDef(p,s1) -> eval_influenceGraph s1 (fst (eval_predecessor p lv lm)) (snd (eval_predecessor p lv lm))
		in
	let identify_path e =
		match e with
			| Parser_ig.InfluenceGraph(s) -> InfluenceGraph(eval_influenceGraph(s) [] [])
	in identify_path e
;;



let rec create_var_list varl = 
	match varl with
		| [] -> raise(Invalid_argument "create_var_list: Error, none variable found!")
		| (v,_)::[] -> [v]
		| (v,_)::t -> v::(create_var_list t)
;;



let rec create_mult_list multl =
	let rec multformula_to_string mf =
		match mf with
		| MAtom(v,i) -> MAtom(v,i)
		| MPropUn(n,mf1) -> MPropUn(n,multformula_to_string mf1)
		| MPropBin(op,mf1,mf2) -> MPropBin(op,(multformula_to_string mf1),(multformula_to_string mf2))
	in
	let rec mult_list ml =
		match ml with
			| [] -> raise(Invalid_argument "mults_to_string: None multiplex defined !")
			| (m,mf)::[] -> [(m,(multformula_to_string mf))]
			| (m,mf)::t -> (m,(multformula_to_string mf))::(create_mult_list t)
	in mult_list multl
;;


let create_IG varl =	
	let rec identify_resource rl =
		match rl with
			| [] -> []
			| h::[] -> [h]
			| h::t -> h ::(identify_resource t)
		in
	let rec identify_ig vl =
		match vl with
			| [] -> raise(Invalid_argument "identify_ig: None variable defined !")
			| (v,(n,rl))::[] -> [(v,((int_of_string n),(identify_resource rl)))]
			| (v,(n,rl))::t -> (v,((int_of_string n),(identify_resource rl)))::(identify_ig t)
		in identify_ig varl 
;;

(* TODO : TEMPORARY PRINT *)

let print_ig ig =	
	let rec identify_resource rl =
		match rl with
			| [] -> ""
			| h::[] -> "\"" ^ h ^ "\""
			| h::t -> "\"" ^ h ^ "\";" ^ (identify_resource t)
		in
	let rec identify_ig vl =
		match vl with
			| [] -> raise(Invalid_argument "identify_ig: None variable defined !")
			| (v,(n,rl))::[] -> "(\"" ^ v ^ "\",(" ^ string_of_int n ^ ",[" ^  (identify_resource rl) ^ "]))" 
			| (v,(n,rl))::t -> "(\"" ^ v ^ "\",(" ^ string_of_int n ^ ",[" ^ (identify_resource rl) ^ "])); " ^ (identify_ig t)
		in identify_ig ig 
;;


let rec print_vars vars =
	match vars with
		| [] -> ""
		| h::[]-> "\"" ^ h ^ "\""
		| h::t -> "\"" ^ h ^ "\"," ^ (print_vars t)
;;

let print_mults mults =
	let rec multformula_to_string mf =
		match mf with
		| MAtom(v,i) -> "MAtom(\"" ^ v ^ "\"," ^ (string_of_int i) ^ ")"
		| MPropUn(n,mf1) -> "MPropUn(" ^ string_of_neg n ^ "," ^ multformula_to_string mf1 ^ ")"
		| MPropBin(op,mf1,mf2) -> "MPropBin(" ^ string_of_operator op ^ "," ^ (multformula_to_string mf1) ^ "," ^ (multformula_to_string mf2) ^ ")"
	in
	let rec mult_list ml =
		match ml with
			| [] -> raise(Invalid_argument "mults_to_string: None multiplex defined !")
			| (m,mf)::[] -> "(\"" ^ m ^ "\"," ^ (multformula_to_string mf) ^ ")"
			| (m,mf)::t -> "(\"" ^ m ^ "\"," ^ (multformula_to_string mf) ^ ");" ^ (mult_list t)
	in mult_list mults
;;





let rec identify_influence_graph l =
	match l with
		| InfluenceGraph(ig, mults)::[] -> (create_IG ig, (create_var_list ig, create_mult_list mults))
		| InfluenceGraph(ig, mults)::t -> (create_IG ig, (create_var_list ig, create_mult_list mults))
		| [] -> raise(Invalid_argument "identify_ig: None influence graph identified !");;




(* Function which does the parsing of the influence graph.
The global extraction_data variable contains information
about the influence graph, the variables and the 
multiplexes defined in the influence graph. *)
let extraction_data = ref [];;
let rec parse oe =
	let lexbuf = Lexing.from_channel (oe) in
	try
		while true do
			let token = Parser_ig.program Lexer_ig.token lexbuf in
			extraction_data := [eval_ig token] @ !extraction_data;
		done		
	with Lexer_ig.Eof ->
		printf("")
;;


(* Main function for parsing the input file and define a new
syntax for the influence graph, mandatory for the simulator *)
let parse_ig =
	let filename = Sys.argv.(1) in
	(* printf "Input file name : %s\n" filename; *)
	let oe = open_in filename in
	try 
		parse oe;
	with End_of_file -> ()
;;



