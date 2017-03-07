open AST

let build_method_table c_ast c_name =
    let rec iter c_name methods =
        match methods with
            | [] -> Env.initial()
            | m::others -> let env = (iter c_name others) and key = c_name^"_"^m.mname
                           in if Env.mem env key then Error.environment_duplicate key
                           else Env.define env key m
    in iter c_name c_ast.cmethods

let build_object_descriptor c_ast c_name =
    let rec iter attributes =
        match attributes with
            | [] -> Env.initial()
            | a::others -> let env = (iter others) and key = a.aname
                           in if Env.mem env key then Error.environment_duplicate key
                           else (Env.define (iter others) key a)
    in c_name, iter c_ast.cattributes

let rec concat_method_tables = function
    | [] -> Env.initial()
    | method_table::others -> Env.merge method_table (concat_method_tables others) 

let rec build_object_descriptor_table = function
    | [] -> Env.initial()
    | (c_name, object_descriptor)::others -> let env = (build_object_descriptor_table others) and key = c_name
                           in if Env.mem env key then Error.environment_duplicate key
                           else Env.define env key object_descriptor

let parser ast f =
  let type_parse t = match t.info with
    | Class(c) -> f c t.id
    | Inter -> Error.not_implemented "Interface compilation" Location.none
  in let rec type_list_parse = function
     | [] -> []
     | h::t -> (type_parse h)::(type_list_parse t)
  in type_list_parse ast.type_list

let execute lexbuf verbose = 
  let init ast =
    let method_table = concat_method_tables (parser ast build_method_table)
    and object_descriptor_table = build_object_descriptor_table (parser ast build_object_descriptor)
    in (method_table, object_descriptor_table)
  in
  try 
   (* MAIN ALGORITHM *)
    let ast = Parser.compilationUnit Lexer.token lexbuf in
    print_endline "successfull parsing";
    if verbose then AST.print_program ast;
    let (method_table, object_descriptor_table) = init ast in
    Typing.execute method_table object_descriptor_table;
    print_endline "successfull typing check";
    (* Starting execution *)
    print_endline "\n EXECUTION : "; 
    Execution.find_main method_table;
    print_endline "\n END EXECUTION : ";
    (* End execution *)
    (* END - MAIN ALGORITHM *)
  with 
    | Parser.Error ->
      print_string "Syntax error: ";
      Location.print (Location.curr lexbuf)
    | Error.Error(e,l) ->
      Error.report_error e;
      Location.print l