let rec exp_typing exp =
  match exp.edesc with
  (* | New of string option * string list * expression list
  | NewArray of Type.t * (expression option) list * expression option
  | Call of expression option * string * expression list *)
  | Attr(e,_) -> { edesc=exp.edesc, etype=e.etype }
 (* | If of expression * expression * expression
  | Val of value
  | Name of string
  | ArrayInit of expression list
  | Array of expression * (expression option) list
  | AssignExp of expression * assign_op * expression
  | Post of expression * postfix_op
  | Pre of prefix_op * expression *)
  | Op(e1,op,e2) -> { edesc=exp.edesc, etype=(infix_typing e1 op e2) }
(*  | CondOp of expression * expression * expression
  | Cast of Type.t * expression
  | Type of Type.t *)
  | ClassOf(t) -> { edesc=exp.edesc, etype=t }
  | Instanceof -> { edesc=exp.edesc, etype=Primitive(Boolean) }
  | VoidClass -> { edesc=exp.edesc, etype=Void }

let rec infix_typing e1 op e2 = 
    (* TODO *)