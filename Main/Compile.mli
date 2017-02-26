(* Build a method table for one class *)
val build_method_table : AST.astclass -> string -> (string, AST.astmethod) Env.t

(* Build an object descriptor for one object *)
val build_object_descriptor : AST.astclass -> string -> string * (string, AST.astattribute) Env.t

(* Apply a function of type [AST.astclass -> string -> 'a] on each class of the AST.
 * Return the list of the results.
 *)
val parser : AST.t -> (AST.astclass -> string -> 'a) -> 'a list

(*
 * Entry point.
 *)
val execute : Lexing.lexbuf -> bool -> unit