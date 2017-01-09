type package_declaration = Package of string

type import_declaration =
    | SingleTypeImport
    | TypeImportOnDemand
    | SingleStaticImport
    | StaticImportOnDemand

type type_declaration =
    | Class
    | Interface

type ast = package_declaration option * import_declaration list

let print_declaration = function
    | Package(s) -> print_string s
    | _ -> ()


let rec print_imports = function
    | [] -> ()
    | i::t -> match i with
            | SingleTypeImport -> print_string "1"
            | TypeImportOnDemand -> print_string "2"
            | SingleStaticImport -> print_string "3"
            | StaticImportOnDemand -> print_string "4";
            print_imports t


let print_ast = function
    | (Some(p),i) -> print_string ("package"); print_imports i
    | (None,i) -> (); print_imports i