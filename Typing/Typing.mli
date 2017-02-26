(* Fill the mutable field expression.etype *)
val exp_typing : AST.expression -> 'a -> 'b -> ('a, (string, AST.astattribute) Env.t) Env.t -> unit

(* 
 *  Check the correction of all methods' bodies.
 *)
val execute : (string, AST.astmethod) Env.t -> (string, (string, AST.astattribute) Env.t) Env.t -> unit
