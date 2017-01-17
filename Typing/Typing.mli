exception MalformedExpression of string
exception EnvironmentException of string

val def_class : AST.t.type_list -> (string,(string, Type.t)Env.t) Env.t

(* Fill the mutable field expression.etype *)
val exp_typing : expression -> expression
