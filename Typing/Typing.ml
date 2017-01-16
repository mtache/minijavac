exception MalformedExpression of string;


let rec exp_typing exp =
    let t=match exp.edesc with
  (*
  | Call of expression option * string * expression list ## 15.12 doc
   *)
  | NewArray of Type.t * (expression option) list * expression option
  | ArrayInit(l)          -> match l with
                                | [] -> raise MalformedExpression("Empty array")
                                | [e] -> Array(exp_typing e,List.length l)
                                | h::t -> Array(exp_typing h,List.length l) (* TODO Improve *)

  | Name(id)              -> Ref(Type.mk_type [] id)
  | Attr(o,id)            -> Ref(Type.mk_type (InnerParser.listOfNames_form_exp o) id)
  | Array(e,el)           -> Array(InnerParser.listOfTypes_form_exp e,List.length el)

  | New(None,p,_)         -> Ref(mk_type p)   (* TODO Improve *)
  | New(Some o,p,_)       -> Ref(mk_type p o)

  | Val(v)                -> val_typing v
  | If(c,e1,e2)           -> if_typing c e1 e2
  | CondOp(c,e1,e2)       -> if_typing c e1 e2 (* check if it is the same case than if *)
  | Op(e1,op,e2)          -> infix_typing e1 op e2
  | AssignExp(e1,op,e2)   -> assign_typing e1 op e2
  | Post(e,op)            -> postfix_typing e op
  | Pre(op,e)             -> prefix_typing op e
  | Cast(t,e)             -> if (e.etype == t) then t  (* Incomplete *)
                                else raise MalformedExpression("Malformed cast") 
  | Type(t)               -> t
  | ClassOf(t)            -> t
  | Instanceof            -> Primitive(Boolean)
  | VoidClass             -> Void
  in { edesc=exp.edesc, etype=Some(t) }

let rec infix_typing e1 op e2 = match op with    (* TODO *)
  | Op_cor   -> if (e1.etype!=Primitive(Boolean)||e2.etype!=Primitive(Boolean)) 
                        then raise MalformedExpression("|| operator with no boolean types")
                        else Primitive(Boolean)
  | Op_cand  -> if (e1.etype!=Primitive(Boolean)||e2.etype!=Primitive(Boolean)) 
                        then raise MalformedExpression("&& operator with no boolean types")
                        else Primitive(Boolean)
  | Op_or    -> "|"
  | Op_and   -> "&"
  | Op_xor   -> "^"
  | Op_eq    -> if (e1.etype==Void)||e2.etype==Void) 
                        then raise MalformedExpression("== operator with void")
                        else Primitive(Boolean)
  | Op_ne    -> if (e1.etype==Void)||e2.etype==Void) 
                        then raise MalformedExpression("!= operator with void")
                        else Primitive(Boolean)
  | Op_gt    -> ">"
  | Op_lt    -> "<"
  | Op_ge    -> ">="
  | Op_le    -> "<="
  | Op_shl   -> "<<"
  | Op_shr   -> ">>"
  | Op_shrr  -> ">>>"
  | Op_add   -> "+"
  | Op_sub   -> "-"
  | Op_mul   -> "*"
  | Op_div   -> "/"
  | Op_mod   -> "%"

let rec assign_typing e1 op e2 = match op with    (* TODO *)
  | Assign  -> "="
  | Ass_add -> "+="
  | Ass_sub -> "-="
  | Ass_mul -> "*="
  | Ass_div -> "/="
  | Ass_mod -> "%="
  | Ass_shl -> "<<="
  | Ass_shr -> ">>="
  | Ass_shrr-> ">>>="
  | Ass_and -> "&="
  | Ass_xor -> "^="
  | Ass_or  -> "|="

let rec prefix_typing e op = match op with    (* TODO *)
  | Op_not -> "!"
  | Op_neg -> "-"
  | Op_incr -> "++"
  | Op_decr -> "--"
  | Op_bnot -> "~"
  | Op_plus -> "+"

let rec postfix_typing op e = match op with     (* TODO *)
  | Incr    ->
  | Decr    ->

let rec if_typing c e1 e2 =
    (* TODO *)
let rec val_typing v =
    (* TODO *)
let rec array_typing e l =
    (* TODO *)
