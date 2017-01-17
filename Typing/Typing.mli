(*  *)
exception MalformedExpression of string

(* Raised when a method or a class is definied two times *)
exception EnvironmentException of string

(*
 *  Creates a class definition environment where first key is
 *  the class name and the second key is the method name.
 *  The value is the type of the method in the declaration.
 *) 
val class_env : AST.t -> (string, (string, Type.t) Env.t) Env.t

(* Fill the mutable field expression.etype
 *
  val exp_typing : AST.expression -> AST.expression
 *)

(* 
 *  Check the correction of the body's method of each class.
 *  Return true if suceeded.
 *)
val check_class : AST.t -> bool
