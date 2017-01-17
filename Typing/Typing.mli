exception MalformedExpression of string
exception EnvironmentException of string

val class_env : AST.t -> (string, (string, Type.t) Env.t) Env.t

(* Fill the mutable field expression.etype *)
(* val exp_typing : AST.expression -> AST.expression *)
