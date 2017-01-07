type declaration =
    | Package of string
    | Import
    | Type

type ast = declaration option


let print_declaration = function
    | Package(s) -> print_string s
    | _ -> ()

let print_ast = function
    | Some(d) -> print_declaration d
    | None -> ()
