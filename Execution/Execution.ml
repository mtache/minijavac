open AST
open Type

(* Pour trouver la méthode main dans method_table *)
let split_main truc =
	let s = Str.split  (Str.regexp "_") truc  in
	match s with
		| h::z::q -> z


(* Some printing functions to print the memory content *)

let print_all_type_of_mem mem =
	Env.iter (fun (key, value) -> print_endline key) mem

let print_all_variable_of_given_type given_type mem =
	let found_env = Env.find mem given_type in
	print_all_type_of_mem found_env

let print_memory_bis env =
  let print_var_env env =
    print_endline " Variable :";
    Env.iter (fun (key,value) ->
    print_endline ("    "^key^" of value "^(string_of_int value))) env
  in Env.iter (fun (key,value) -> 
  print_endline ("Type : "^key);
  print_var_env value) env

(* Some functions to add elements on the memory AND return memory *)

let add_new_type_to_mem t mem =
	let new_env = Env.initial() in
	Env.define mem t new_env

let get_sub_env_by_type type_given mem =
	Env.find mem type_given


let add_new_variable_to_mem variable_type value name mem =
	if (Env.mem mem variable_type)
		then
			(Env.define mem variable_type (Env.define (get_sub_env_by_type variable_type mem) name value))
		else 
			let new_mem = add_new_type_to_mem variable_type mem in
			Env.define new_mem variable_type (Env.define (get_sub_env_by_type variable_type new_mem) name value)


let modify_variable_mem variable_type new_value name mem =
	Env.replace mem variable_type (Env.replace (get_sub_env_by_type variable_type mem) name new_value)

(* Some functions to deal with the dark type from the AST *)



let get_string_type_from_typet obscur_type =
	Type.stringOf obscur_type
	


let get_value_of_exp exp =
	match exp with
		| Val x -> match x with
			| Int n -> int_of_string n
			| _ -> print_endline "not implemented"; 0
		| _ -> print_endline "not implemented"; 0

(* The functions to execute a variable declaration *)


let execute_vardecl_aux one_vd mem= match one_vd with
	| (type_ast, name, Some(exp_value)) -> 
		let variable_type = get_string_type_from_typet type_ast in
		let expdesc_of_exp = exp_value.edesc in
		let var_value = get_value_of_exp expdesc_of_exp in
		add_new_variable_to_mem variable_type var_value name mem
	| _ -> print_endline "not implemented"; mem



let rec execute_vardecl vd_list mem = match vd_list with
	| [] -> print_endline "vardecl finished or empty"; mem
	| h::q -> 
		let new_mem = execute_vardecl_aux h mem in
				execute_vardecl q new_mem 


(* the MAIN algorithm : *)


let execute_statement statement mem=
	match statement with
		| VarDecl dl -> execute_vardecl dl mem 
		| _ -> print_endline "not implemented"; mem 


(* Prends en argument la liste des statements de la methode en cours d execution, ainsi que la memoire qui a ete initialisee *)
let rec execute_statements  statement_list mem = 
	match statement_list with
		| [] -> print_endline "No more statement to execute \n";
			print_endline "Here is the printing of the memory :";
			mem
		| h::t -> print_endline "Starting executing statements"; 
				let new_mem = execute_statement h mem in
				execute_statements t new_mem

(* Lancement de l execution sur la methode main detectee *)
let start_point astmethod_main =
	let mem = Env.initial() in
	let statement_list_main = astmethod_main.AST.mbody in
		execute_statements statement_list_main mem 
	

(* Fonction pour chercher la methode main dans method table *)
let find_main env =
  	Env.iter (fun (key,value) ->  match (split_main key) with 
								|  "main" -> let res =start_point value in
									print_memory_bis res;
									()
								| _ -> () 
	) env 

(* end *)