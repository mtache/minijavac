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

let print_memory env =
  let print_var env =
    print_endline " Variable :";
    Env.iter (fun (key,value) ->
    print_endline ("    "^key^" of value "^(value))) env
  in Env.iter (fun (key,value) ->
  print_endline ("Type : "^key);
  print_var value) env

(* Some functions to add elements on the memory AND return memory *)

let add_new_type_to_mem t mem =
	let new_env = Env.initial() in
	(print_endline "creation new type";
	Env.define mem t new_env )

let get_sub_env_by_type type_given mem =
	Env.find mem type_given


let add_new_variable_to_mem variable_type value name mem =
	if (Env.mem mem variable_type) then
		(print_endline "test1";
		print_memory mem;
		(Env.replace mem variable_type (Env.define (get_sub_env_by_type variable_type mem) name value));)
	else
		let new_mem = add_new_type_to_mem variable_type mem in
		(print_endline "test2";
		print_memory new_mem;
		Env.replace new_mem variable_type (Env.define (get_sub_env_by_type variable_type new_mem) name value);)


let modify_variable_mem variable_type new_value name mem =
	Env.replace mem variable_type (Env.replace (get_sub_env_by_type variable_type mem) name new_value)

(* Some functions to deal with the dark type from the AST *)


(* TODO : deal with each type diffrently *)
let get_string_type_from_typet obscur_type =
	Type.stringOf obscur_type


	let float_operation_exec e1 op e2 =
			let val1 = float_of_string e1 in
			let val2 = float_of_string e2 in
			match op with
				| Op_add ->
										string_of_float (val1 +. val2)
				| Op_sub ->
										string_of_float (val1 -. val2)
				| Op_mul ->
										string_of_float (val1 *. val2)
				| Op_div ->
										string_of_float (val1 /. val2)

		(* Function to execute a simple operation *)
let boolean_operation_exec e1 op e2 =
		let val1 = bool_of_string e1 in
		let val2 = bool_of_string e2 in
			match op with
			| Op_cand -> string_of_bool ( val1 && val2)
			| Op_cor -> string_of_bool ( val1 || val2)

let int_operation_exec e1 op e2 =
		let val1 = int_of_string e1 in
		let val2 = int_of_string e2 in
		match op with
			| Op_add -> print_endline ("Added"^string_of_int (val1+ val2));
									string_of_int (val1+ val2)

			| Op_sub -> print_endline ("sub"^string_of_int (val1- val2));
									string_of_int (val1- val2)

			| Op_mul -> print_endline ("mul"^string_of_int (val1* val2));
									string_of_int (val1 * val2)

			| Op_div -> print_endline ("div"^string_of_int (val1/val2));
									string_of_int (val1 / val2)

			(*| Op_mod -> None
			| Op_cor -> None
			| Op_cand -> None
			| Op_or -> None
			| Op_and -> None
			| Op_xor -> None
			| Op_eq -> None
			| Op_ne -> None
			| Op_gt -> None
			| Op_lt -> None
			| Op_ge -> None
			| Op_le -> None
			| Op_shl -> None
			| Op_shr -> None
			| Op_shrr -> None *)



			let execute_op e1 inf_op e2 exp_type=
			match exp_type with
				| None -> print_endline "Type not found"; "Not found"
				| Some(e) -> 			match e with
														| Primitive prim -> match prim with
																	| Int ->  int_operation_exec e1 inf_op e2
																	| Float -> float_operation_exec e1 inf_op e2
																	| Boolean -> boolean_operation_exec e1 inf_op e2
																	| _ -> "Unimplemented"
														| _ -> "Uninplemented"
(* The functions to execute a variable declaration *)

(* TODO : add all the type of exp *)
let rec get_value_of_exp exp=
	let exp_desc = exp.edesc in
	let exp_type = exp.etype in
	match exp_desc with
		| Op(e1, inf_op, e2) ->
				(execute_op (get_value_of_exp e1) inf_op (get_value_of_exp e2) exp_type)
		| Val x -> match x with
			| Int n -> n
			| Float f -> f
			| Boolean b -> string_of_bool b
			| _ -> print_endline "not implemented"; "0"
		| _ -> print_endline "not implemented"; "0"

let execute_vardecl_aux one_vd mem= match one_vd with
	| (type_ast, name, Some(exp_value)) ->
		let variable_type = get_string_type_from_typet type_ast in
		let expdesc_of_exp = exp_value.edesc in
		let given_type = exp_value.etype in
		let var_value = get_value_of_exp exp_value  in
		(match given_type with
				| None -> print_endline "lollololol"
				| Some(type_found) -> print_endline ("LOOK HERE"^(Type.stringOf type_found)));
		add_new_variable_to_mem variable_type var_value name mem
	| _ -> print_endline "not implemented"; mem



let rec execute_vardecl vd_list mem = match vd_list with
	| [] -> print_endline "vardecl finished or empty"; mem
	| h::q ->
		let new_mem = execute_vardecl_aux h mem in
				(execute_vardecl q new_mem )


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
									print_memory res;
									()
								| _ -> ()
	) env

(* end *)
