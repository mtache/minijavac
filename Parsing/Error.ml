open AST

type t =
  | Illegal_character of char
  | Illegal_escape_char
  | Unterminated_string
  | Unterminated_comment
  | Syntax
  | Wrong_return of astmethod
  | Non_numeric_operand of expression * infix_op
  | Non_boolean_operand of expression * infix_op
  | Malformed_expression of expression
  | Environment_duplicate of string
  | Not_implemented of string
  | Unknown_attribute of string
  | Unknown_method of string
  | Wrong_throw of astmethod
  
exception Error of t * Location.t;;

(* Les erreurs. *)
let report_error = function
  | Illegal_character c ->
      print_string "Illegal character (";
      print_char c;
      print_string "): "
  | Illegal_escape_char ->
      print_endline "Illegal escape character in string: "
  | Unterminated_string ->
      print_endline "String literal not terminated: "
  | Unterminated_comment ->
      print_endline "Comment not terminated: "
  | Syntax ->
      print_endline "Syntax error: "
  | Wrong_return m ->
      print_endline ("Wrong return type in method: "^m.mname)
  | Non_numeric_operand (e,op) ->
      print_endline ("The operand "^(string_of_expression e)^" is used with the operator "^(string_of_infix_op op)^" and is not convertible to primitive numeric type: ")
  | Non_boolean_operand (e,op) ->
      print_endline ("The operand "^(string_of_expression e)^" is used with the operator "^(string_of_infix_op op)^" and is not of boolean type: ")
  | Malformed_expression e ->
      print_endline ("The expression "^(string_of_expression e)^" is malformed: ")
  | Environment_duplicate key ->
      print_endline (key^" is defined more than once")
  | Not_implemented s ->
      print_endline ("This has not been implemented : "^s)
  | Unknown_method m -> print_endline ("Unknown method : "^m)
  | Unknown_attribute a ->
      print_endline ("Unknown attribute : "^a)
  | Wrong_throw m ->
      print_endline ("Wrong throw block in method: "^m.mname)

let illegal_char char loc =
  raise (Error(Illegal_character char, loc))

let illegal_escape_char loc =
  raise (Error(Illegal_escape_char, loc))

let unterminated_string loc =
  raise (Error (Unterminated_string, loc))

let unterminated_comment loc =
  raise (Error (Unterminated_comment, loc))

let syntax loc =
  raise (Error (Syntax, loc))

let wrong_return m =
  raise (Error (Wrong_return m, m.mloc))

let non_numeric_operand e op =
  raise (Error (Non_numeric_operand (e,op), e.eloc))

let non_boolean_operand e op =
  raise (Error (Non_boolean_operand (e,op), e.eloc))

let malformed_expression e =
  raise (Error (Malformed_expression e, e.eloc))

let environment_duplicate k =
  raise (Error (Environment_duplicate k, Location.none))

let not_implemented k l =
  raise (Error (Not_implemented k, l))

let unknown_attribute id loc =
  raise (Error (Unknown_attribute id, loc))

let unknown_method id loc =
  raise (Error (Unknown_method id, loc))
let wrong_throw m =
  raise (Error (Wrong_throw m, m.mloc))