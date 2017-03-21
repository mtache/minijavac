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
		((* print_endline "test1";
		print_memory mem ;*)
		(Env.replace mem variable_type (Env.define (get_sub_env_by_type variable_type mem) name value));)
	else
		let new_mem = add_new_type_to_mem variable_type mem in
		((* print_endline "test2";
		print_memory new_mem; *)
		Env.replace new_mem variable_type (Env.define (get_sub_env_by_type variable_type new_mem) name value);)


(* two functions to modify variables in memory. The function "bis" does not need the type of the variable *)
let modify_variable_mem_bis var_id new_value mem =
	let res = ref (Env.initial()) in
	let type_key = ref "" in
  	let second_round env var_id new_value var_type= Env.iter (fun (key,value) ->
  		if (key = var_id) then ( type_key := var_type; res := (Env.replace env var_id new_value); )) env in
	(Env.iter (fun (key,value) -> second_round value var_id new_value key) mem;
	Env.replace mem !type_key !res)

let modify_variable_mem variable_type new_value name mem =
	Env.replace mem variable_type (Env.replace (get_sub_env_by_type variable_type mem) name new_value)

(* return the type of a variable, given just its name *)
let get_type_of_variable_from_mem var_id mem =
let res = ref "" in
  let second_round env var_id type_env= Env.iter (fun (key,value) ->
  	if (key = var_id) then res := type_env else () ) env in
 (Env.iter (fun (key,value) -> second_round value var_id key) mem;
  !res;)

(* return the value of the varaible, given just its name *)
let get_variable_from_mem var_id mem =
  let res = ref "" in
  let second_round env var_id = Env.iter (fun (key,value) ->
  	if (key = var_id) then res := value else () ) env in
 (Env.iter (fun (key,value) -> second_round value var_id ) mem;
  !res;)





(* Some functions to deal with the dark type from the AST *)


(* TODO : deal with each type diffrently *)
let get_string_type_from_typet obscur_type =
	Type.stringOf obscur_type


let float_operation_exec e1 op e2 =
		let val1 = float_of_string e1 in
		let val2 = float_of_string e2 in
		match op with
			| Op_add -> string_of_float (val1 +. val2)
			| Op_sub -> string_of_float (val1 -. val2)
			| Op_mul -> string_of_float (val1 *. val2)
			| Op_div -> string_of_float (val1 /. val2)
			| Op_lt ->  string_of_bool (val1 < val2)
			| Op_le ->  string_of_bool (val1 <= val2)
			| Op_gt ->  string_of_bool (val1 > val2)
			| Op_ge ->  string_of_bool (val1 >= val2)
			| Op_eq ->  string_of_bool (val1 = val2)

(* Function to execute a simple operation *)
let boolean_operation_exec e1 op e2 =
		let val1 = bool_of_string e1 in
		let val2 = bool_of_string e2 in
			match op with
			| Op_cand -> string_of_bool ( val1 && val2)
			| Op_cor -> string_of_bool ( val1 || val2)
			| Op_eq -> string_of_bool (val1 = val2)

let execute_op e1 inf_op e2 exp_type=
match exp_type with
	| None -> print_endline "Type not found"; "Not found"
	| Some(e) -> 			match e with
											| Primitive prim -> match prim with
														| Int ->  float_operation_exec e1 inf_op e2
														| Float -> float_operation_exec e1 inf_op e2
														| Boolean -> boolean_operation_exec e1 inf_op e2
														| _ -> "Unimplemented"
											| _ -> "Uninplemented"





(* The functions to execute a variable declaration *)

let get_variable_name_of_exp_desc exp_desc mem =
	match exp_desc with
		| Name truc -> truc
		| _ -> "Not implemented"

(* TODO : add all the type of exp *)
let rec get_value_of_exp exp mem =
	let exp_desc = exp.edesc in
	(* let exp_type = exp.etype in *)
	match exp_desc with
		| Op(e1, inf_op, e2) ->
		let exp_type = e1.etype in
				(execute_op (get_value_of_exp e1 mem) inf_op (get_value_of_exp e2 mem) exp_type)
		| Val x -> (match x with
			| Int n -> n
			| Float f -> f
			| Boolean b -> string_of_bool b
			| _ -> print_endline "not implemented"; "0")
		| Name var_id -> get_variable_from_mem var_id mem
		| _ -> print_endline "not implemented"; "0"


(* executing the statements of type expression *)
let execute_expression exp mem =
		let ex_desc = exp.edesc in
			match ex_desc with
				| AssignExp(e1,op,e2) -> match e1.etype with
						| Some(ex_type) -> 	let ex_type_bis = (Type.stringOf ex_type ) in begin
							 match op with
							 	| Assign -> modify_variable_mem_bis (get_variable_name_of_exp_desc e1.edesc mem) (get_value_of_exp e2 mem)  mem
							 	| Ass_add -> let new_value = execute_op (get_value_of_exp e1 mem) Op_add (get_value_of_exp e2 mem) e1.etype in
							 		modify_variable_mem_bis (get_variable_name_of_exp_desc e1.edesc mem) (new_value)  mem
								| Ass_sub -> let new_value = execute_op (get_value_of_exp e1 mem) Op_sub (get_value_of_exp e2 mem) e1.etype in
							 		modify_variable_mem_bis (get_variable_name_of_exp_desc e1.edesc mem) (new_value)  mem
								| Ass_mul -> let new_value = execute_op (get_value_of_exp e1 mem) Op_mul (get_value_of_exp e2 mem) e1.etype in
							 		modify_variable_mem_bis (get_variable_name_of_exp_desc e1.edesc mem) (new_value)  mem
								| Ass_div -> let new_value = execute_op (get_value_of_exp e1 mem) Op_div (get_value_of_exp e2 mem) e1.etype in
							 		modify_variable_mem_bis (get_variable_name_of_exp_desc e1.edesc mem) (new_value)  mem
								| _ -> print_endline "Not implemented"; mem
							end
						| None -> print_endline "not implemented"; mem
																	 (* "0" *)
			(* | Op(e1, inf_op, e2) -> *)

let execute_vardecl_aux one_vd mem= match one_vd with
	| (type_ast, name, Some(exp_value)) ->
		let variable_type = get_string_type_from_typet type_ast in
		let expdesc_of_exp = exp_value.edesc in
		let given_type = exp_value.etype in
		let var_value = get_value_of_exp exp_value mem in
		(* (match given_type with
				| None -> print_endline "lollololol"
				| Some(type_found) -> print_endline ("LOOK HERE"^(Type.stringOf type_found))); *)
		add_new_variable_to_mem variable_type var_value name mem
	| _ -> print_endline "not implemented"; mem



(* Executing the statements of type "variable declaration" *)

let rec execute_vardecl vd_list mem = match vd_list with
	| [] -> (* print_endline "vardecl finished or empty"; *) mem
	| h::q ->
		let new_mem = execute_vardecl_aux h mem in
				(execute_vardecl q new_mem )


(* the MAIN algorithm : *)
let rec execute_statement statement mem=
	match statement with
		| VarDecl dl -> execute_vardecl dl mem
		| Expr exp ->  execute_expression exp mem
		| While(e1, s1 ) ->
			(
				let rec while_loop e1 s1 mem =
					if bool_of_string (get_value_of_exp e1 mem);
					then ( print_endline "Starting while statement";
									while_loop e1 s1  (execute_statement s1 mem))
					else  mem
					in while_loop e1 s1 mem
			)

		| If(e1, stat1, stat2) ->
			(
				print_endline "Starting If statement";
				let bool_e1 = get_value_of_exp e1 mem in
				if (bool_of_string bool_e1)
					then (execute_statement stat1 mem)
				else begin
					match stat2 with
					| Some(stat) -> execute_statement stat mem
					| None -> mem
				end
			)
		| Block(stat_list) ->
			(
				print_endline "Starting Block statement";
				match stat_list with

				| [] -> mem
				| h::q ->
					let new_mem = (execute_statement h mem) in
					let test_one = Block(q) in
					execute_statement test_one new_mem 
			)
		| For(dec_list, bool_limit, inc_op_list, stat) ->
			(* Instanciation of the variable which will move - tested OK *)
			let new_mem_aux vardec mem =
				match vardec with
					| (Some(type_ast), name, Some(exp_value)) ->
						let variable_type = get_string_type_from_typet type_ast in
						let expdesc_of_exp = exp_value.edesc in
						let given_type = exp_value.etype in
						let var_value = get_value_of_exp exp_value mem in
						add_new_variable_to_mem variable_type var_value name mem
					| (None, name, Some(exp_value)) -> modify_variable_mem_bis name (get_value_of_exp exp_value mem) mem
					| _ -> print_endline "not implemented"; mem
			in
			(* Instanciation of the variable in the memory which will move - tested OK *)
			let rec new_mem dec_list mem = 
				(match dec_list with
					| [] -> mem
					| h::q -> 
						(let new_mem_for = new_mem_aux h mem in
						let new_mem_res = new_mem q new_mem_for in
						begin
							new_mem_res
						end)
				)
			(* Incrementation of the variable - tested OK *)		
			in
			let rec incr inc_op_list mem =
				(
					match inc_op_list with
						| [] -> mem
						| h::q -> 
							let exp_to_stat = Expr(h) in
							(
								let updated_mem = execute_statement exp_to_stat mem in
								(
									incr q updated_mem
								)
							)
				)
			in
			(* modification of the memory during one iteration *)
			let modif_mem inc_op_list stat mem =
				let m1 = execute_statement stat mem in
				(
					let m2 = incr inc_op_list m1 in
					(
						m2
					)
				)
			in
			(* To evaluate the condition of the end of the for statement *)
			let bool_of_limit bool_limit mem =
				(
					match bool_limit with
						| Some(e)-> bool_of_string (get_value_of_exp e mem)
						| None -> true
				)
			in
			(* The main algorithm to execute the for statement *)
			let rec exec_for  bool_limit inc_op_list stat mem =
				(
					let bool_res = bool_of_limit bool_limit mem in
					(
						match bool_res with
								| true -> 
									let updated_mem = modif_mem inc_op_list stat mem in
									(
										exec_for bool_limit inc_op_list stat updated_mem
									)
								| false -> mem
					)
				)
			in
			(* initializing the memory and executing *)
			let starting_mem = new_mem dec_list mem in
			(
				exec_for bool_limit inc_op_list stat starting_mem
			)
		(* If bot implemented ... *)
		| _ -> print_endline "not implemented"; mem


(* Prends en argument la liste des statements de la methode en cours d execution, ainsi que la memoire qui a ete initialisee *)
let rec execute_statements  statement_list mem =
 match statement_list with
		| [] -> print_endline "\nNo more statement to execute \n";
			print_endline "\nHere is the printing of the memory :\n";
			mem
		| h::t -> (* print_endline "\nStarting executing statements\n"; *)
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
