type name = SimpleName of string | QualifiedName of string list

type import =
    | SingleTypeImport of name
    | TypeImportOnDemand of name
    | SingleStaticImport of name
    | StaticImportOnDemand of name

type package = Package of name

type modifier =
    | Public
    | Protected
    | Private
    | Abstract
    | Static
    | Final
    | Strictfp

type class_declaration = modifier list * string * string option * string option

type interface_declaration = modifier list * string

type type_declaration = 
    | Class of class_declaration
    | Interface of interface_declaration

type ast = package option * import list * type_declaration list

(* Unused tyoes *)
type expression = Expression of string

type methodnode =
    {
      name : string;
      body : expression;
    }

type classnode =
    {
      name : string;
      methods : methodnode list;
    }
(* END - Unused types *)

(* Print functions *)
let print_string_option = function
    | Some(s) -> print_string s
    | None -> print_string ""

let rec name_string = function
    | QualifiedName(h::[]) -> h
    | QualifiedName(h::t) -> (name_string (QualifiedName(t)))^"."^h

let print_package = function
    | Package(p) -> print_string ("package "^(name_string p)^"\n")
    | _ -> ()

let rec print_imports = function
    | [] -> ()
    | i::t -> begin match i with
            | SingleTypeImport(i) -> print_string ("SingleTypeImport "^name_string i)
            | TypeImportOnDemand(i) -> print_string ("TypeImportOnDemand "^name_string i)
            | SingleStaticImport(i) -> print_string ("SingleStaticImport "^name_string i)
            | StaticImportOnDemand(i) -> print_string ("StaticImportOnDemand "^name_string i)
            end;
            print_imports t

let rec print_modifiers = function
    | [] -> ()
    | i::t -> begin match i with
            | Public -> print_string "public "
            | Protected -> print_string "protected "
            | Private -> print_string "private "
            | Abstract -> print_string "abstract "
            | Static -> print_string "static "
            | Final -> print_string "final "
            | Strictfp -> print_string "strictfp "
            end;
            print_modifiers t

let print_class = function
    | (m,n,s,i) -> print_modifiers m; print_string "class "; print_string n; print_string " extends "; print_string_option s; print_string " implements "; print_string_option i; print_string "\n"

let print_interface = function
    | (m,s) -> print_modifiers m; print_string "interface "; print_string s; print_string "\n"

let rec print_types = function
    | [] -> ()
    | i::t -> begin match i with
            | Class(c) -> print_class c
            | Interface(i) -> print_interface i
            end;
            print_types t

let print_ast = function
    | (Some(p),i,t) -> print_package p; print_imports i; print_types t
    | (None,i,t) -> print_imports i; print_types t
(* END - Print functions *)