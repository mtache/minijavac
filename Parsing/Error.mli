(* The error type with its associated exception for the compiler *)
type t
exception Error of t * Location.t

(* print an error *)
val report_error : t -> unit

(* raise the various errors *)
val illegal_char : char -> Location.t -> 'a
val illegal_escape_char : Location.t -> 'a
val unterminated_string : Location.t -> 'a
val unterminated_comment : Location.t -> 'a
val syntax : Location.t -> 'a
val wrong_return : AST.astmethod -> 'a
val non_numeric_operand : AST.expression -> AST.infix_op -> 'a
val non_boolean_operand : AST.expression -> AST.infix_op -> 'a
val malformed_expression : AST.expression -> 'a
val environment_duplicate : string -> 'a
val not_implemented : string -> Location.t-> 'a
val unknown_attribute : string -> Location.t -> 'a
val unknown_method : string -> Location.t -> 'a
val wrong_throw : AST.astmethod -> 'a
