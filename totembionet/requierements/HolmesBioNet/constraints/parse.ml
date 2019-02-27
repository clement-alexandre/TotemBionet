(********************************)
(*** Parser of the input file ***)
(********************************)

(* This program reads the input file containing hybrid Hoare logic.
Starting from an input file, a parser is used in order to identify
the influence graph, the hoare Triple and if this path is cyclic *)

open Types;;
open Printf;;
open Display;;


(*******************************)
(*** Identification of types ***)
(*******************************)

(* Definition of the different types (Influence Graph, Hoare Triple
and Cyclic behaviour) given by the parser *)
let eval_hoareTriple e =
	let eval_duration t =
		match t with
			| Parser.ValueTime(vt) -> ValueTime(float_of_string(vt))
			| Parser.VarTime(vt) -> VarTime(vt)
		in 
	let rec eval_symbol_slide s =
		match s with
			| '+' -> PlusSlide
			| '-' -> MinusSlide
			| _ -> raise(Invalid_argument "eval_symbol : Should have the '+', '-' symbol for Slide or none")
		in
	let eval_comp c =
		match c with
			| "<="|"=<" -> LE
			| "<" -> LT
			| ">="|"=>" -> GE
			| ">" -> GT
			| "=" -> Eq
			| "!=" -> NEq
			| _ -> raise(Invalid_argument "eval_comp: Need a comparator for the Celerities of assertions")
		in
	let rec eval_resources w = 
		match w with
			| Parser.Multiplex(m) -> [m]
			| Parser.SetMultiplexes(m,r) -> [m] @ eval_resources(r)
		in
	let eval_symbol_hterm s =
		match s with
			| '+' -> Plus
			| '-' -> Minus
			| '*' -> Times
			| '/' -> Divide
			| _ -> raise(Invalid_argument "eval_symbol_hterm : Should have +, -,* or / symbol in relation between Hybrid terms")
		in
	let rec eval_hybridTerm ht =
		match ht with
			| Parser.PiEntrance(p) -> PiEntrance(p,0)
			| Parser.PiExit(p) -> PiExit(p,0)
			| Parser.Celerity(v,w,n) -> Celerity(v,eval_resources(w),int_of_string(n))
			| Parser.CelerityNR(v,n) -> Celerity(v,[],int_of_string(n))
			| Parser.HTermRelation(ht1,connective,ht2) -> HTermRelation(eval_symbol_hterm(connective),eval_hybridTerm(ht1),eval_hybridTerm(ht2))
			| Parser.HConstant(cst) -> HConstant(float_of_string(cst))
			| Parser.HTime(t) -> HTime(eval_duration(t))
		in
	let rec eval_assertion ea =
		match ea with
			| Parser.ASlide(v)-> ASlide(EqSlide,v)
			| Parser.ANoSlide(v)-> ANoSlide(EqSlide,v)
			| Parser.ASlideSymbol(s,v)-> ASlide(eval_symbol_slide(s),v)
			| Parser.ANoSlideSymbol(s,v)-> ANoSlide(eval_symbol_slide(s),v)
			| Parser.ACelerity(v,comp,value)-> ACelerity(eval_comp(comp),v,eval_hybridTerm(value))
			| Parser.ABoolean(a)-> ABoolean(boolean_of_string(a))
			| Parser.AOpAnd(a1,a2)-> AOp(And,eval_assertion(a1),eval_assertion(a2))
			| Parser.AOpOr(a1,a2)-> AOp(Or,eval_assertion(a1),eval_assertion(a2))
			| Parser.ANeg(a)-> ANeg(Neg,eval_assertion(a))
			| Parser.APar(a)-> eval_assertion(a)
		in
	let eval_symbol s =
		match s with
			| '+' -> PlusSymbol
			| '-' -> MinusSymbol
			| _ -> raise(Invalid_argument "eval_symbol : Should have + or - symbol for the discrete path atom")
		in
	let eval_dpa d =
		match d with
			| Parser.DpaValue(v,s) -> (v,eval_symbol(s))
		in
	let eval_elementaryPath ep =
		match ep with
			| Parser.Tuple(d,at,dpa) -> Tuple(eval_duration(d),eval_assertion(at),eval_dpa(dpa))
		in
	let eval_symbol_dterm s =
		match s with
			| '+' -> DPlus
			| '-' -> DMinus
			| _ -> raise(Invalid_argument "eval_symbol_dterm : Should have + or - symbol in relation between Discrete terms")
		in
	let eval_discreteTerm d =
		match d with
			| Parser.Eta(v) -> Eta(v)
			| Parser.DConstant(v) -> DConstant(int_of_string(v))
		in
	let rec eval_discreteAtom d =
		match d with
			| Parser.DTermUn(dt) -> eval_discreteTerm(dt)
			| Parser.DTermRelation(dt1,connective,dt2) -> DTermRelation(eval_symbol_dterm(connective),eval_discreteAtom(dt1),eval_discreteAtom(dt2)) 
		in
	let rec eval_discreteCondition d =
		match d with
			| Parser.DBoolean(c1) -> DBoolean(boolean_of_string(c1))
			| Parser.DAtom(d1,comp,d2) -> DAtom(eval_comp(comp),eval_discreteAtom(d1),eval_discreteAtom(d2))
			| Parser.DOpAnd(d1,d2) -> DOp(And,eval_discreteCondition(d1),eval_discreteCondition(d2))
			| Parser.DOpOr(d1,d2) -> DOp(Or,eval_discreteCondition(d1),eval_discreteCondition(d2))
			| Parser.DNeg(d1) -> DNeg(Neg,eval_discreteCondition(d1))
			| Parser.DPar(d1) -> eval_discreteCondition(d1)
		in
	let rec eval_hybridCondition c =
		match c with
			| Parser.HBoolean(h1) -> HBoolean(boolean_of_string(h1))
			| Parser.HAtom(h1,comp,h2) -> HAtom(eval_comp(comp),eval_hybridTerm(h1),eval_hybridTerm(h2))
			| Parser.HDAtom(hd1,comp,hd2) -> HDAtom(eval_comp(comp),eval_discreteAtom(hd1),eval_discreteAtom(hd2))
			| Parser.HOpAnd(h1,h2) -> HOp(And,eval_hybridCondition(h1),eval_hybridCondition(h2))
			| Parser.HOpOr(h1,h2) -> HOp(Or,eval_hybridCondition(h1),eval_hybridCondition(h2))
			| Parser.HNeg(h1) -> HNeg(Neg,eval_hybridCondition(h1))
			| Parser.HPar(h1) -> eval_hybridCondition(h1)
		in
	let rec eval_path p =
		match p with
			| Parser.EPath(ep) -> eval_elementaryPath(ep)
			| Parser.Seq(ep,p) -> Seq(eval_elementaryPath(ep),(eval_path p))
		in
	let eval_post p =
		match p with
			| Parser.PostCondition(dc,hc) -> Postcondition(eval_discreteCondition(dc),eval_hybridCondition(hc))
		in
	let rec eval_multformula m =
		match m with
			| Parser.MAtom(v,comp,i) -> (match comp with
						| "=" -> MPropBin(And,MAtom(v,int_of_string(i)),MPropUn(Neg,MAtom(v,int_of_string(i)+1)))
						| "!=" -> MPropUn(Neg,MPropBin(And,MAtom(v,int_of_string(i)),MPropUn(Neg,MAtom(v,int_of_string(i)+1))))
						| ">" -> MAtom(v,int_of_string(i)+1)
						| ">="|"=>" -> MAtom(v,int_of_string(i))
						| "<" -> MPropUn(Neg,MAtom(v,int_of_string(i)))
						| "<="|"=<" -> MPropUn(Neg,MAtom(v,int_of_string(i)+1))
						| _ -> raise(Invalid_argument "eval_Mcomp : not a comparator !"))
			| Parser.MPropNeg(m1) -> MPropUn(Neg,eval_multformula(m1)) 
			| Parser.MPropAnd(m1,m2) -> MPropBin(And,eval_multformula(m1),eval_multformula(m2))
			| Parser.MPropOr(m1,m2) -> MPropBin(Or,eval_multformula(m1),eval_multformula(m2))
			| Parser.MPropImpl(m1,m2) -> MPropBin(Or,MPropUn(Neg,eval_multformula(m1)),eval_multformula(m2))
			| Parser.MPropPar(m1) -> eval_multformula(m1)
		in
	let eval_formula f =
		match f with
			| Parser.MFormula(f1) -> eval_multformula(f1)
		in
	let rec eval_varlist v =
		match v with
			| Parser.MVar(v1) -> [v1]
			| Parser.MVarList(v1,v2) -> [v1] @ eval_varlist(v2)
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
			| Parser.MTarget(t1) -> eval_varlist(t1)
		in
	let eval_predecessor p lv lm =
		match p with
			| Parser.VarDefinition(v,n) -> (lv @ [(v,((int_of_string n),[]))] , lm)
			| Parser.MultDefinition(m,f,t) ->  (create_list_target lv m (eval_target t), lm @ [(m,eval_formula(f))])
		in
	let rec eval_influenceGraph s lv lm =
		match s with
			| Parser.PredecessorDef(p) -> eval_predecessor p lv lm
			| Parser.SeveralVarDef(p,s1) -> eval_influenceGraph s1 (fst (eval_predecessor p lv lm)) (snd (eval_predecessor p lv lm))
		in
	let identify_path e =
		match e with
			| Parser.HoareTriple(path,post) -> HoareTriple(eval_path(path),eval_post(post))
			| Parser.InfluenceGraph(s) -> InfluenceGraph(eval_influenceGraph(s) [] [])
			| Parser.IsCyclic(boolean) -> IsCyclic(boolean)
	in identify_path e
;;


(*********************************************)
(*** Functions which write the output file ***)
(*********************************************)

let rec identify_ig l =
  match l with
    | [] -> raise(Invalid_argument "identify_ig: Influence Graph not recognized !")
    | InfluenceGraph(ig)::[] -> ig
    | h::[] -> raise(Invalid_argument "identify_ig: Influence Graph not recognized !")
    | InfluenceGraph(ig)::t -> ig
    | h::t -> identify_ig t
;;

let rec identify_ht l =
  match l with
    | [] -> raise(Invalid_argument "identify_ht: Hoare Triple not recognized !")
    | HoareTriple(path, Postcondition(d_init, h_init))::[] -> (path, (d_init, h_init))
    | h::[] -> raise(Invalid_argument "identify_ht: Hoare Triple not recognized !")
    | HoareTriple(path, Postcondition(d_init, h_init))::t -> (path, (d_init, h_init))
    | h::t -> identify_ht t
;;

let rec identify_cyclic_behaviour l =
  match l with
    | [] -> raise(Invalid_argument "identify_cyclic_behaviour: Don't know if the path describes a cyclic behaviour")
    | IsCyclic(b)::[] -> b
    | h::[] -> false
    | IsCyclic(b)::t -> b
    | h::t -> identify_cyclic_behaviour t
;;



(************************************)
(*** Main functions of the parser ***)
(************************************)


(* Read the file with the lexer and parser, and create an output file
containing the variables of the graph, the multiplexes defined,
the path with the appropriate type for the next functions *)
let input_data = ref [];;

let rec parse oe =
	let lexbuf = Lexing.from_channel (oe) in
	try
		while true do
			let token = Parser.program Lexer.token lexbuf in
			input_data := [eval_hoareTriple token] @ !input_data
		done	
	with Lexer.Eof ->
		(* print_endline "Reading input file achieved !" *)
		print_endline ""
;;

(* Main function which parse the input file *)
let _ =
	let filename = Sys.argv.(1) in
	(* printf "Input file name : %s\n" filename; *)
	let oe = open_in filename in
	try 
		parse oe;
	with End_of_file -> ()
;;



