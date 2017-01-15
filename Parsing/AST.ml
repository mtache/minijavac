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


(* Unused tyoes *)
(* type expression = Expression of string

type methodnode =
    {
      name : string;
      body : expression list;
    } *)
(* END - Unused types *)

(* Expressions *)
type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod

type unop =
  | Uplus | Uminus

type const =
  | Float of float
  | Int of int


type basicType =
  | ByteType
  | ShortType
  | IntType
  | LongType
  | CharType
  | BooleanType
  | FloatType
  | DoubleType

type operation =
  | BooleanOperation
  | Bool of bool
  | Const of const
  | Var of string
  | Binop of binop * operation * operation
  | Unop of unop * operation

type ifStatement =
	| IfThenElse of operation * statement * statement
	| IfThen of operation * statement

and forStatement =
	| BasicFor of statement option * operation option * statement option * statement option
	| EnhancedFor

and switch_statement = Switch of operation * switch_case list

and switch_case =
  | Default_case of statement
  | Normal_case of operation * statement

and statement =
  | Declaration of basicType * string * operation option
  | IfStatement of ifStatement
  | ForStatement of forStatement
  |  While of operation * statement
  | DoWhile of operation  * statement
  | SwitchStatement of switch_statement
  | Operation of operation


  type expr =
  	| Statement of statement

    (* ROOT *)
  type ast =
    | Formule of package option * import list * types list
    | Expression of expr list


(* type ourast = expr list *)



exception Unbound_variable of string

let get_op_u = function
  | Uplus -> fun x -> x
  | Uminus -> fun x -> -x


let string_of_op_u = function
  | Uplus -> "+"
  | Uminus -> "-"

let string_of_op_b = function
  | Badd -> "Badd"
  | Bsub -> "Bsub"
  | Bmul -> "Bmul"
  | Bdiv -> "/"
  | Bmod -> "%"


let string_of_const = function
  | Float n -> " FLOAT :"^(string_of_float n)^" "
  | Int n  -> " INT : "^(string_of_int n)^" "

let string_of_bool = function
  | true -> "BOOLEAN TRUE"
  | false -> "BOOLEAN FALSE"

let string_of_basicType = function
  | ByteType -> " ByteType "
  | ShortType -> " ShortType "
  | IntType -> " IntType "
  | LongType -> " LongType "
  | CharType -> " CharType "
  | BooleanType -> " BooleanType "
  | FloatType -> " FloatType "
  | DoubleType -> " DoubleType "

(*)
let rec eval env exp =
  match exp with
  | Const c -> c
  | Var v -> (try List.assoc v env with Not_found -> raise(Unbound_variable v))
  | Binop(op,e1,e2) -> (get_op_b op) (eval env e1) (eval env e2)
  | Unop(op,e) -> (get_op_u op) (eval env e)
*)



let rec string_of_operation op =
  match op with
  | Const c -> string_of_const c
  | Var v -> "VAR "^v
  | Binop(op, e1, e2) ->
      "(" ^(string_of_operation e1)^ (string_of_op_b op) ^(string_of_operation e2)^ ")"
  | Unop(op, e) -> "(" ^ (string_of_op_u op) ^(string_of_operation e)^ ")"
  | Bool b -> string_of_bool b

(*
and rec string_of_switch_case_list = function
  | [] -> ""
  | t::q -> begin match t with
            | Default_case(s) -> " Default: "^print_statement(s)^" \n"^string_of_switch_case_list(q)
            | Normal_case(op, s) -> " case :"^string_of_operation(op)^" : "^print_statement(s)^" \n"^string_of_switch_case_list(q)
            end
*)

let print_variable = function
|(d,v) -> print_string ("DECLARATION ("^d ^ "," ^string_of_int v ^")")



let rec print_statement = function
    | Declaration(bt, id, Some(op)) -> "Declaration avec valeur "^string_of_basicType(bt)^id^string_of_operation(op)
    | Declaration(bt, id, None) -> "Declaration sans valeur "^string_of_basicType(bt)^id
    | While(op, st) -> " WHILE ("^string_of_operation(op)^")\n{"^print_statement st ^ "}\n"
    | DoWhile(op, st) -> "DO{ \n"^print_statement st ^ ";\n} WHILE (" ^ string_of_operation(op)^"); \n"
  	| IfStatement(i) -> begin match i with
  			| IfThenElse(op, e1, e2) -> " IF ("^string_of_operation(op)^") \n {"^print_statement(e1)^"} \nELSE {"^print_statement(e2) ^"}\n"
			  | IfThen(op, e1) -> " IF ("^string_of_operation(op)^")\n { "^print_statement(e1) ^"}\n"
  			end
  	| ForStatement(f) -> begin match f with
  			| BasicFor(Some(forinit),Some(condition),Some(forupdate),Some(action)) -> " FOR (init : "^print_statement(forinit)^" condition : "^string_of_operation(condition)^" update : "^print_statement(forupdate)^" DO "^print_statement(action)
  			end
  	|SwitchStatement(s) -> begin match s with
        | Switch(op,switch_case_list) ->" SWITCH ("^string_of_operation(op)^") \n"
          end
    | Operation (o) -> string_of_operation(o)


let rec print_expression = function
	| [] -> ""
	| i::t -> begin match i with
				| Statement(s) -> print_statement(s)^print_expression(t)
				end




let print_ast = function
   | i -> print_expression i



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
    | Formule (Some(p),i,t) -> print_package p; print_imports i; print_types t
    | Formule (None,i,t) -> print_imports i; print_types t
    | Expression (i) -> print_string (print_expression i)
(* END - Print functions *)
