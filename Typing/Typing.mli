(*
 *  Creates a class definition environment where first key is
 *  the class name and the second key is an environment where the
 *  first key is the method name.
 *  The value of the second environment is the method type 
 *  in the declaration.
 *) 
val class_env : AST.t -> (string, (string, Type.t) Env.t) Env.t


(* Fill the mutable field expression.etype *)
val exp_typing : AST.expression -> AST.expression

(* 
 *  Check the correction of the body's method of each class.
 *  Return true if suceeded.
 *)
val check_class : AST.t -> bool
