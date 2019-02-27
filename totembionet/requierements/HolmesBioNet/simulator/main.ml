open Parse_ig;;
open Parse_cel;;
open Parse_dinit;;
open Parse_piinit;;
open Simulation;;
open Output_file;;


let _ =
	parse_ig;
	let (ig,(vars, mults)) = identify_influence_graph !extraction_data in
	(* print_endline("[" ^ (print_ig ig) ^ "]");
	print_endline("[" ^ (print_vars vars) ^ "]");
	print_endline("[" ^ (print_mults mults) ^ "]"); *)
	parse_cel;
	let celerities = !extraction_cel in
	(* print_endline(print_celList celerities);	 *)
	(* let (dinit, piinit) = parse_init Sys.argv.(3) in  *)
	(* let dinit = [("u", 0); ("v", 0)] in
	let piinit = [("u", 1.); ("v", 0.)] in *)
	parse_dinit;
	let dinit = !extraction_dinit in
	(* print_endline(print_eta dinit); *)
	parse_piinit;
	let piinit = !extraction_piinit in
	(* print_endline(print_pi piinit); *)
	let tmax = float_of_string (Sys.argv.(5)) in
	let init_trace = [(0.0, (dinit, piinit))] in
	let final_trace = simulation(ig, vars, mults, dinit, piinit, celerities, 0., tmax, init_trace, 0.) in
	create_file final_trace (Sys.argv.(6)) vars;;
	(* print_endline("Time\tvar1\tvar2");
	print_endline(print_trace final_trace vars);; *)
	(* print_endline(print_trace2 final_trace);; *)
;;


(* ligne de commande :
simu graph cel dinit piinit tmax
 *)
(* Le main parse le graphe, les célérités, l'état initial
Puis on lance la simulation
et écrit dans le fichier de sortie *)