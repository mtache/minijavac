(* 
 *  Check the correction of all methods' bodies.
 *  Parameters are method table and object descriptor table.
 *)
val execute : (string, AST.astmethod) Env.t -> (string, (string, AST.astattribute) Env.t) Env.t -> unit
