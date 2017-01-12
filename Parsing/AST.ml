type qualified_name = string list

type package_declaration = Package of qualified_name

type import_declaration =
    | SingleTypeImport
    | TypeImportOnDemand
    | SingleStaticImport
    | StaticImportOnDemand

type modifier =
    | Public
    | Protected
    | Private
    | Abstract
    | Static
    | Final
    | Strictfp

type class_declaration = modifier list * string

type interface_declaration = modifier list * string

type type_declaration = 
    | Class of class_declaration
    | Interface of interface_declaration

type ast = package_declaration option * import_declaration list * type_declaration list

let rec print_qualified_name = function
    | [] -> ()
    | i::t -> print_string i;
              print_qualified_name t

let print_declaration = function
    | Package(s) -> print_string "package"; print_qualified_name s
    | _ -> ()


let rec print_imports = function
    | [] -> ()
    | i::t -> begin match i with
            | SingleTypeImport -> print_string "Single type import"
            | TypeImportOnDemand -> print_string "Type import on demand"
            | SingleStaticImport -> print_string "Single static import"
            | StaticImportOnDemand -> print_string "Static import on demand"
            end;
            print_imports t

let rec print_modifiers = function
    | [] -> ()
    | i::t -> begin match i with
            | Public -> print_string "public"
            | Protected -> print_string "protected"
            | Private -> print_string "private"
            | Abstract -> print_string "abstract"
            | Static -> print_string "static"
            | Final -> print_string "final"
            | Strictfp -> print_string "strictfp"
            end;
            print_modifiers t

let print_class = function
    | (m,s) -> print_modifiers m; print_string "class"; print_string s

let print_interface = function
    | (m,s) -> print_modifiers m; print_string "interface"; print_string s

let rec print_types = function
    | [] -> ()
    | i::t -> begin match i with
            | Class(c) -> print_class c
            | Interface(i) -> print_interface i
            end;
            print_types t



let print_ast = function
    | (Some(p),i,t) -> print_declaration p; print_imports i; print_types t
    | (None,i,t) -> (); print_imports i; print_types t