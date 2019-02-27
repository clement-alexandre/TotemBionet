
open Types;;

(*******************************************************
Description interne d'un état: 
eta : [(v_1,val); (v_2,val) ...]
pi  : [(v_1,piv_1); (v_2,piv_2); ...]
  
celerities : [("C_v__m1_m2__2",val); ...]

ig : [("v", (b_v, ["m1";"m3"])); ("u", (b_u, ["m2"; "m3"])); ...]
mults : [("m1", MPropBin(...)); ("m2", MPropUn(Neg,...)); ...]
vars : ["v"; "u"; ...]
*******************************************************)


let getpredec a ig = snd (List.assoc a ig) ;;
let getformula m mults = List.assoc m mults ;;
let getbound a ig = fst (List.assoc a ig) ;;


(* TODO: temporary print for debugging *)
let rec print_of_lv lv =
  match lv with
    [] -> ""
    | [v] -> v
    | v::l -> v ^ ";" ^ print_of_lv l;;

let rec print_of_newpi newpi =
  match newpi with
    [] -> ""
    | [(v,value)] -> v ^ ":" ^ string_of_float value
    | (v,value)::l -> v ^ ":" ^ string_of_float value ^ ";" ^ print_of_newpi l;;

let rec print_of_d d =
  match d with
    [] -> ""
    | [(v,value)] -> v ^ ":" ^ string_of_int value
    | (v,value)::l -> v ^ ":" ^ string_of_int value ^ ";" ^ print_of_d l;;

(*******************************************
  Part 1 :
  search of celerities in a given domain...
 *******************************************)


(* compute the value of a variable at a specified discrete state *)
let valueOfAt(v,d) = List.assoc v d;;

(* evaluate a formula at a specified discrete state *)
let rec evaluate_at(multformula,d)=
  match multformula with
    | MAtom(v,i) -> (List.assoc v d) >= i
    | MPropUn(n,m) -> not (evaluate_at(m,d))
    | MPropBin(op, m1, m2) -> 
      match op with
        | And -> (evaluate_at(m1,d)) && (evaluate_at(m2,d))
        | Or -> (evaluate_at(m1,d)) || (evaluate_at(m2,d))
        | Impl -> (not (evaluate_at(m1,d))) || (evaluate_at(m2,d))
;; 
(* Booleen qui vérifie formule correcte dans etat discret *)


(* is a multiplex a re source of a variable at a given discrete state ? *)
let is_a_ressource_of_at(mults,multiplex,var,d)=
  let formula = getformula multiplex mults in
  evaluate_at(formula,d);;

(* the resources of a variable at a given discrete state *)
let rec the_resources_amongst(mults, listMult, var, d)=
  match listMult with
    [] -> []
  | m::l -> if is_a_ressource_of_at(mults,m,var,d) then
      m::the_resources_amongst(mults, l, var, d)
  else the_resources_amongst(mults, l, var, d);;

let theResourcesOfAt(mults, var, d, ig)=
  let preds = getpredec var ig in
  List.sort compare (the_resources_amongst(mults, preds, var, d));;

(* building of the string representing the celerity depending 
   of a variable, a set of resources and of a level *)
let rec string_of_ressources(ress) =
  match ress with
    [] -> "__"
  | [r1] -> r1 ^ "__"
  | r1::ll -> r1 ^ "_" ^ string_of_ressources(ll);;
  
let buildCelerityName(var,ress,d) =
  "C_" ^ var ^ "__" ^ string_of_ressources(ress) ^ string_of_int(valueOfAt(var,d));;

(* The useful function : 
   returns the value of the current celerity of a variable *)

let theCelerityOfAt(mults,var,d,ig,all_cels)=
  let ress =  theResourcesOfAt(mults, var, d, ig) in
  let celerityName = buildCelerityName(var,ress,d) in 
  valueOfAt(celerityName,all_cels);;



(* Crée une liste de tuple comprenant chaque variable et la 
valeur de la célérité associée à cette variable dans l'état 
discret courant  *)
let rec theCeleritiesAt(mults,d,variables,ig,all_cels)=
  match variables with
    [] -> []
  | [v] -> [(v,theCelerityOfAt(mults,v,d,ig,all_cels))]
  | v1::l -> (v1,theCelerityOfAt(mults,v1,d,ig,all_cels))::theCeleritiesAt(mults,d,l,ig,all_cels);;

				    
(*******************************************
  Part 2 :
  search of the touch delays for each variable
 *******************************************)

let theTouchDelayOfAt(v,d,pi,celerities)= 
  let celv = valueOfAt(v,celerities) in 
  let piv = valueOfAt(v,pi) in
  (* print_endline("\n" ^ "v = " ^ v ^ ", cel = " ^ string_of_float celv ^ ", pi = "^ string_of_float piv);
  (* print_endline("\t"^print_of_d d); *)
  print_endline("si cel>0, alors touch_delay = : " ^ string_of_float((1.0-.piv)/.celv));
  print_endline("si cel<0, alors touch_delay = : " ^ string_of_float((0.0-.piv)/.celv)); *)
  if celv == 0.0 then infinity
  else if celv > 0.0 then (1.0-.piv)/.celv
  else (0.-.piv)/.celv;;


(* Crée une liste de tuple comprenant chaque variable
associée au temps passé dans l'état *)
let rec theTouchDelaysAt(variables,d,pi,celerities)=
  (* print_endline("tTDAt:"); *)
  (* print_endline("d:" ^ print_of_d d);
  print_endline("pi:" ^ print_of_newpi pi);
  print_endline("cel:" ^ print_of_newpi celerities);
  print_endline(""); *)  
  match variables with
    [] -> []
  | [v] -> [(v,theTouchDelayOfAt(v,d,pi,celerities))]
  (* | [v] -> print_endline("var:" ^ v ^ ", td:" ^ string_of_float (valueOfAt(v,celerities)) ^ "\t" ^ string_of_float (theTouchDelayOfAt(v,d,pi,celerities)));[(v,theTouchDelayOfAt(v,d,pi,celerities))] *)
  | v1::l -> (v1,theTouchDelayOfAt(v1,d,pi,celerities))::theTouchDelaysAt(l,d,pi,celerities);;
  (* | v1::l -> print_endline("var:" ^ v1 ^ ", td:" ^ string_of_float (valueOfAt(v1,celerities)) ^ "\t" ^ string_of_float (theTouchDelayOfAt(v1,d,pi,celerities)));(v1,theTouchDelayOfAt(v1,d,pi,celerities))::theTouchDelaysAt(l,d,pi,celerities);; *)


(*******************************************
  Part 3 :
  Continuous transitions
 *******************************************)
let epsilon = 1e-12;;

let rec voisin d v n = 
  match d with
   [] -> raise (Invalid_argument "voisin: Error")
   | [(v1,value)] -> if (v = v1) then [(v1, (value + n))] else (raise (Invalid_argument "voisin: Error"))
   | (v1,value)::l -> if (v = v1) then (v1, (value + n)) :: l else (v1, value) :: (voisin l v n);;


let a_variable_faces_a_wall ig mults v d pi celerities all_cels =
  (* print_endline("face_a_wall:"); *)
  (* print_endline("var:" ^ v); *)
  let dv = List.assoc v d in
  (* print_endline("dv:" ^ string_of_int dv); *)
  let piv = List.assoc v pi in
  (* print_endline("piv:" ^ string_of_float piv); *)
  let celv = List.assoc v celerities in
  (* print_endline("celv:" ^ string_of_float celv); *)
  if ((piv < 1. -. epsilon) && (piv > epsilon))
  then false
  else
    if ((dv = getbound v ig) && (celv > epsilon) && (abs_float (piv -. 1.) < epsilon)) || 
        ((dv = 0) && (celv < ~-.epsilon) && (piv < epsilon))
    then true
    else
      if ((abs_float (piv -. 1.) < epsilon) && (celv > epsilon))
      then 
        let dbis = voisin d v 1 in
        let celbis = theCelerityOfAt(mults,v,dbis,ig,all_cels) in 
        celbis < epsilon
      else
        if ((piv < epsilon) && (celv < ~-.epsilon))
        then 
          let dbis = voisin d v (-1) in
          let celbis = theCelerityOfAt(mults,v,dbis,ig,all_cels) in 
          celbis > -.epsilon
        else false;;

(* Identification de toutes les sliding variables dans un état hybride *)
let rec sv ig mults all_vars d pi celerities all_cels=
  (* print_endline("sv: t"); *)
  match all_vars with
    [] -> []
    | v::[] -> if (a_variable_faces_a_wall ig mults v d pi celerities all_cels) then [v] else []
    (* | v::[] -> print_endline(string_of_bool (a_variable_faces_a_wall ig mults v d pi celerities all_cels) ); if (a_variable_faces_a_wall ig mults v d pi celerities all_cels) then [v] else [] *)
    | v::l -> if (a_variable_faces_a_wall ig mults v d pi celerities all_cels) then ([v] @ (sv ig mults l d pi celerities all_cels)) else (sv ig mults l d pi celerities all_cels);;
    (* | v::l -> print_endline(string_of_bool (a_variable_faces_a_wall ig mults v d pi celerities all_cels) ); if (a_variable_faces_a_wall ig mults v d pi celerities all_cels) then ([v] @ (sv l d pi celerities)) else (sv l d pi celerities);; *)

let is_in_sv v slv = List.mem v slv;;



(* On recherche toutes les variables qui touchent leur bord en premier *)
let rec calcul_des_min touchDelayList =
  match touchDelayList with
    [] -> raise(Invalid_argument "calcul_des_min: List of touch Delay empty")
  | [(v, t)] -> ([v], t)
  | (v, t)::tail ->
    let (lvt, ttail) = calcul_des_min tail in
    if ((abs_float (t -. ttail)) < epsilon) then ([v] @ lvt, ttail) 
    else if (t < ttail -. epsilon) then ([v], t) else (lvt, ttail)
;;


(* Build the new positions inside the current discrete state *)
let rec build_new_pi(vars, pi, celerities, touchmin, sv) =
  match vars with
   [] -> []
  | v::[] -> 
    let piv = List.assoc v pi in
    let celv = List.assoc v celerities in
    if (List.mem v (fst touchmin)) || (is_in_sv v sv)
    then
      if (celv > epsilon) then ([(v, 1.)]) else ([(v, 0.)])
    else
      [(v, piv +. (celv *. (snd touchmin)))]
  | v::l -> 
    let piv = List.assoc v pi in
    let celv = List.assoc v celerities in
    if (List.mem v (fst touchmin)) || (is_in_sv v sv)
    then
      if (celv > epsilon) 
      then ((v, 1.)::(build_new_pi(l, pi, celerities, touchmin, sv))) 
      else ((v, 0.)::(build_new_pi(l, pi, celerities, touchmin, sv)))
    else
    (v, piv +. (celv *. (snd touchmin)))::(build_new_pi(l, pi, celerities, touchmin, sv));;


(* Identify all segments inside the current discrete state *)
let aLineSegment(ig,mults,all_vars,varlist,d,pi,celerities) = 
  (* print_endline("\n");
  print_endline("aLineSegment:"); *)
  let cel_in_state = theCeleritiesAt(mults,d,all_vars,ig,celerities) in
  (* print_endline("cel_in_state: " ^ print_of_newpi cel_in_state); *)
  let sliding_variables = sv ig mults all_vars d pi cel_in_state celerities in
  let touchdelays = theTouchDelaysAt(varlist,d,pi,cel_in_state) in
  let touchmin = calcul_des_min(touchdelays) in
  (touchmin, build_new_pi(all_vars, pi, cel_in_state, touchmin, sliding_variables));;



let remove_all_elts lv varlist =
  List.filter (fun x -> not (List.mem x lv)) varlist;;


(* Identify the continuous transition inside the current discrete state.
This function also gives the jumping variables when a threshold is reached *)
let rec aContinuousTransition(ig,mults,all_vars,varsl,d,pi,celerities,t)=
  (* print_endline("cT:"); *)
  (* print_endline(string_of_float t) ; *)
  let ((lv, tmin), newpi) = aLineSegment(ig,mults,all_vars,varsl,d,pi,celerities) in
  (* print_endline("tmin: " ^ string_of_float tmin);
  print_endline("lv: " ^ print_of_lv lv);
  print_endline("newpi: " ^ print_of_newpi newpi); *)
  let cel_in_state = theCeleritiesAt(mults,d,varsl,ig,celerities) in
  let sliding_variables = sv ig mults lv d newpi cel_in_state celerities in
  (* print_endline(print_of_lv sliding_variables); *)
  let jumping_variables = List.filter (fun x -> not (List.mem x sliding_variables)) lv in
  (* print_endline(print_of_lv jumping_variables); *)
  (* Tester si toutes les variables de lv (first + sv) sont dans sliding_variables  *)
  if (List.for_all (fun x -> is_in_sv x sliding_variables) lv)
  then
    let new_varlist = remove_all_elts sliding_variables varsl in
    [(jumping_variables, ((tmin+.t), (d, newpi)))] @ aContinuousTransition(ig,mults,all_vars,new_varlist,d,newpi,celerities,(t+.tmin))
  else
    [(jumping_variables, ((tmin+.t), (d, newpi)))];;

(* 
let aContinuousTransition(vars, d, pi, celerities, t) =
  let (jv, l) = aContinuousTransition_aux(vars,d,pi,celerities,t) in
  (jv, (t, (d,pi))::l);;
 *)

(*******************************************
  Part 4 :
  Simulations
 *******************************************)

(*******************************************
Une étable de la boucle de simulation: 
- on commence à time,d,pi
- calcul de la transition continue sous forme de suite d'états 
- calcul d'une transition discrete 
- on recommence...
 ********************************************)


let change_pi v1 sign_cel tuple =
  if v1 = (fst tuple)
  then 
    if sign_cel = 1
    then (v1, 0.)
    else (v1, 1.)
  else tuple;;

let rec voisin_pi new_pi v sign_cel =
  List.map (fun x -> change_pi v sign_cel x) new_pi;;

let choisir_parmi jv =
  (* Remarque: Random.int bound renvoie un entier aléatoire entre 0 et bound exclu.
  bound doit être plus grand que 0 ! *)
  if (List.length jv = 0)
  then raise(Invalid_argument "choisir_parmi: Empty list")
  else
    let random_position = Random.int (List.length jv) in
    List.nth jv random_position;;

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

let rec string_of_dinit d =
  match d with
    | [] -> ""
    | h::t -> (fst h) ^ "=" ^ string_of_int (snd h) ^ ", " ^ (string_of_dinit t)
;;

let rec string_of_piinit d =
  match d with
    | [] -> ""
    | h::t -> (fst h) ^ "=" ^ string_of_float (snd h) ^ ", " ^ (string_of_piinit t)
;;

let rec id_cel vl mults dinit ig celerities =
  match vl with
  | [] -> ""
  | v::t -> "cel_" ^ v ^ "=" ^string_of_float(theCelerityOfAt(mults,v,dinit,ig,celerities)) ^ (id_cel t mults dinit ig celerities)
;;

let rec string_of_ress l =
  match l with
  | [] -> ""
  | v::t -> v ^ ", " ^ string_of_ress t
;;

let rec simulation(ig,all_vars,mults,dinit,piinit,celerities,tinit,tmax,trace, tprev) =
  (* print_endline("simulation:");
  print_endline(string_of_float tmax); *)
  (* print_endline("time:" ^ string_of_float tinit);
  print_endline("time previously:" ^ string_of_float tprev); *)
  (* print_endline("eta" ^ string_of_dinit dinit);
  print_endline("pi" ^ string_of_piinit piinit);
  print_endline("ressource B" ^ string_of_ress(theResourcesOfAt(mults, "B", dinit, ig)));
  print_endline(buildCelerityName("B",theResourcesOfAt(mults, "B", dinit, ig),dinit));
  print_endline("cel" ^ id_cel all_vars mults dinit ig celerities); *)
  if ((tinit = tprev) && (tinit > 0.))
  then
    trace @ [(tmax, (dinit, piinit))]
  else
    (* else identification of the next state ... *)
    if ((abs_float (tinit -. tmax)) < epsilon) || (tinit > tmax)
    then trace
    else 
      (* let (jumping_variables, cT) = aContinuousTransition(vars, dinit, piinit, celerities, tinit) in *)
      let ll = aContinuousTransition(ig, mults, all_vars, all_vars, dinit, piinit, celerities, tinit) in
      let (jumping_variables, (new_time, (new_d, new_pi))) = (List.hd (List.rev ll)) in
      let cT = List.map (fun (x,y) -> y) ll in
      if jumping_variables = []
      then
         trace @ [(tmax, (new_d, new_pi))]
      else
        let choosen_v = choisir_parmi jumping_variables in
        let sign_choosen_cel = 
          let choosen_cel = theCelerityOfAt(mults, choosen_v, new_d, ig, celerities) in
          if choosen_cel > epsilon then 1 else -1 in
        let next_d = voisin new_d choosen_v sign_choosen_cel in
        let next_pi = voisin_pi new_pi choosen_v sign_choosen_cel in
        simulation(ig, all_vars, mults, next_d, next_pi, celerities, new_time, tmax, trace @ cT, tinit);;
