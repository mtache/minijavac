exception DeclarationError of string

type name = Name of string list (*
                                 * Can either be : qualified name, the head is the string after the last point;
                                 * a simple name as a list with a single string.
                                 *)

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

type classnode =
    {
        cmodifiers : modifier list;
        cname : string; (* Use name type ? *)
        cextends : string option; (* TODO use classnode instead *)
        cimplements : string list option; (* TODO use interfacenode instead *)
        (*methods : methodnode list;*)
    }

type interfacenode =
    {
        imodifiers : modifier list;
        iname : string; (* Use name type ? *)
        iextends : string list option; (* TODO use interfacenode instead *)
        (*methods : methodnode list;*)
    }

type types = 
    | Class of classnode
    | Interface of interfacenode

(* ROOT *)
type ast = package option * import list * types list

(* Unused tyoes *)
type expression = Expression of string

type methodnode =
    {
      name : string;
      body : expression list;
    }
(* END - Unused types *)

(* Print functions *)
let option_string s p = match s with
    | Some(s) -> p^s
    | None -> ""

let rec extends_string = function
    | None -> ""
    | Some(h::[]) -> "extends " ^ h
    | Some(h::t) -> extends_string (Some(t)) ^ ", " ^ h

let rec implements_string = function
    | None -> ""
    | Some(h::[]) -> "implements " ^ h
    | Some(h::t) -> h ^ ", " ^ extends_string (Some(t))

let rec name_string = function
    | Name(h::[]) -> h
    | Name(h::t) -> (name_string (Name(t)))^"."^h

let print_package = function
    Package(p) -> print_string ("package "^(name_string p)^"\n")

let rec print_imports = function
    | [] -> ()
    | i::t -> begin match i with
            | SingleTypeImport(i) -> print_string ("SingleTypeImport "^name_string i^"\n")
            | TypeImportOnDemand(i) -> print_string ("TypeImportOnDemand "^name_string i^"\n")
            | SingleStaticImport(i) -> print_string ("SingleStaticImport "^name_string i^"\n")
            | StaticImportOnDemand(i) -> print_string ("StaticImportOnDemand "^name_string i^"\n")
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

let print_class c =
    print_modifiers c.cmodifiers; print_string ("class "^c.cname^(option_string c.cextends " extends ")^(implements_string c.cimplements)^"\n")

let print_interface i =
    print_modifiers i.imodifiers; print_string ("interface "^i.iname^" "^(extends_string i.iextends)^"\n")

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