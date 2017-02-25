(* Fill the mutable field expression.etype *)
val exp_typing : AST.expression -> AST.expression

(* 
 *  Check the correction of the body's method of each class.
 *  Return true if suceeded.
 *)
val check_class : AST.t -> (string, AST.astmethod) Env.t -> (string, (string, AST.astattribute) Env.t) Env.t -> bool
