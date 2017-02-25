open AST

let build_method_table c_ast c_name =
    let rec iter c_name methods =
        match methods with
            | [] -> Env.initial()
            | m::others -> (Env.define (iter c_name others) (c_name^"_"^m.mname) m)
    in iter c_name c_ast.cmethods

let build_object_descriptor c_ast c_name =
    let rec iter attributes =
        match attributes with
            | [] -> Env.initial()
            | a::others -> (Env.define (iter others) a.aname a)
    in c_name, iter c_ast.cattributes

let rec concat_method_tables = function
    | [] -> Env.initial()
    | method_table::others -> Env.merge method_table (concat_method_tables others) 

let rec build_object_descriptor_table = function
    | [] -> Env.initial()
    | (c_name, object_descriptor)::others -> Env.define (build_object_descriptor_table others) c_name object_descriptor

let parser ast f =
  let type_parse t = match t.info with
    | Class(c) -> f c t.id
    | Inter -> Error.not_implemented "Interface compilation" Location.none
  in let rec type_list_parse = function
     | [] -> []
     | h::t -> (type_parse h)::(type_list_parse t)
  in type_list_parse ast.type_list

  let init ast =
    let method_table = concat_method_tables (parser ast build_method_table)
    and object_descriptor_table = build_object_descriptor_table (parser ast build_object_descriptor)
    in (method_table, object_descriptor_table);