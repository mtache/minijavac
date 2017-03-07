open AST

(* Pour trouver la méthode main dans method_table *)
let split_main truc =
	let s = Str.split  (Str.regexp "_") truc  in
	match s with
		| h::z::q -> z

(*  Not finished 
let get_name_vardecl vd = match vd with
	| [] -> print_endline "problem : ";
			""
	| h::q -> 
*)


let execute_statement statement mem=
	match statement with
		| VarDecl dl -> List.iter (fun (t,id,init) ->
			(* TEST : juste un print de la déclaration de variable *)
			print_string((Type.stringOf t)^" "^id);
			(match init with
			| None -> ()
			| Some e -> print_string (" "^(string_of_expression e)));
			print_endline ";"
		       ) dl;
			mem
		| _ -> print_endline "not implemented"; mem


(* Prends en argument la liste des statements de la methode en cours d'execution, ainsi que la memoire qui a ete initialisee *)
let rec execute_statements  statement_list mem = 
	match statement_list with
		| [] -> print_endline "No more statement to execute"; ()
		| h::t -> print_endline "Starting executing statements"; 
				let new_mem = execute_statement h mem in
				execute_statements t new_mem

(* Lancement de l'execution sur la methode main detectee *)
let start_point astmethod_main =
	let mem = Env.initial() in
	let statement_list_main = astmethod_main.AST.mbody in
		execute_statements statement_list_main mem 
	

(* Fonction pour chercher la methode main dans method table *)
let find_main env =
  Env.iter (fun (key,value) ->  match (split_main key) with 
								|  "main" -> start_point value
								| _ -> () ) env 