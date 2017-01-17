exception MalformedExpression of string


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
  | Cast(t,e)             -> match e.etype with (* Incomplete *)
                                | t -> t 
                                | _ -> raise MalformedExpression("Malformed cast") 
  | Type(t)               -> t
  | ClassOf(t)            -> t
  | Instanceof            -> Primitive(Boolean)
  | VoidClass             -> Void
  in { edesc=exp.edesc, etype=Some(t) }

let rec infix_typing e1 op e2 = match op with    (* TODO *)
  | Op_cor   -> match (e1.etype, e2.etype) with 
                | (Primitive(Boolean), Primitive(Boolean)) -> Primitive(Boolean)
                | _ -> raise MalformedExpression("|| operator with non boolean types")
  | Op_cand  -> match (e1.etype, e2.etype) with 
                | (Primitive(Boolean), Primitive(Boolean)) -> Primitive(Boolean)
                | _ -> raise MalformedExpression("&& operator with non boolean types")
  | Op_or    -> "|"
  | Op_and   -> "&"
  | Op_xor   -> "^"
  | Op_eq    -> match (e1.etype, e2.etype) with 
                | (Void, _) -> Primitive(Boolean)
                | (_, Void) -> Primitive(Boolean)
                | _ -> raise MalformedExpression("== operator with void operand")
  | Op_ne    -> match (e1.etype, e2.etype) with 
                | (Void, _) -> Primitive(Boolean)
                | (_, Void) -> Primitive(Boolean)
                | _ -> raise MalformedExpression("!= operator with void operand")
  | Op_gt    -> if check_numeric_operands ">" e1 e2 then begin match (e1.etype, e2.etype) with 
                | (* TODO *)
                end
  | Op_lt    -> if check_numeric_operands "<" e1 e2 then begin match (e1.etype, e2.etype) with 
                | (* TODO *)
                end
  | Op_ge    -> if check_numeric_operands ">=" e1 e2 then begin match (e1.etype, e2.etype) with 
                | (* TODO *)
                end
  | Op_le    -> if check_numeric_operands "<=" e1 e2 then begin match (e1.etype, e2.etype) with 
                | (* TODO *)
                end
  | Op_shl   -> "<<"
  | Op_shr   -> ">>"
  | Op_shrr  -> ">>>"
  | Op_add   -> if check_numeric_operands "+" e1 e2 then begin match (e1.etype, e2.etype) with 
                | (* TODO *)
                end
  | Op_sub   -> if check_numeric_operands "-" e1 e2 then begin match (e1.etype, e2.etype) with 
                | (* TODO *)
                end
  | Op_mul   -> if check_numeric_operands "*" e1 e2 then begin match (e1.etype, e2.etype) with 
                | (* TODO *)
                end
  | Op_div   -> if check_numeric_operands "/" e1 e2 then begin match (e1.etype, e2.etype) with 
                | (* TODO *)
                end
  | Op_mod   -> if check_numeric_operands "%" e1 e2 then begin match (e1.etype, e2.etype) with 
                | (* TODO *)    
                end

let check_numeric_operands opstr e1 e2 =
    match (e1.etype, e2.etype) with 
                | (Primitive(Boolean), Primitive(Boolean)) -> raise MalformedExpression(opstr^" operator with an operand that is not convertible to primitive numeric type")
                | (Primitive, Primitive) -> true
                | _ -> raise MalformedExpression(opstr^" operator with an operand that is not convertible to primitive numeric type")

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
