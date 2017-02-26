open AST
open Type

let rec check_numeric_operand expl op = let check exp = match exp.etype with
  | Some(Primitive(Boolean)) -> Error.non_numeric_operand exp op
  | Some(Primitive(_)) -> true
  | _ -> Error.non_numeric_operand exp op
  in match expl with [] -> true | h::t -> (check h) && (check_numeric_operand t op)

let rec check_boolean_operand expl op = let check exp = match exp.etype with
  | Some(Primitive(Boolean)) -> true
  | _ -> Error.non_boolean_operand exp op
  in match expl with [] -> true | h::t -> (check h) && (check_numeric_operand t op)


let rec infix_typing e1 op e2 = match op with    (* TODO *)
  | Op_cor   -> if check_boolean_operand (e1::[e2]) op then None else None    (* TODO *)
  | Op_cand  -> if check_boolean_operand (e1::[e2]) op then None else None   (* TODO *)
  | Op_or    -> None    (* TODO *)
  | Op_and   -> None    (* TODO *)
  | Op_xor   -> None    (* TODO *)
  | Op_eq    -> None    (* TODO *)
  | Op_ne    -> None    (* TODO *)
  | Op_gt    -> if check_numeric_operand (e1::[e2]) op then None else None     (* TODO *)
  | Op_lt    -> if check_numeric_operand (e1::[e2]) op then None else None     (* TODO *)
  | Op_ge    -> if check_numeric_operand (e1::[e2]) op then None else None     (* TODO *)
  | Op_le    -> if check_numeric_operand (e1::[e2]) op then None else None     (* TODO *)
  | Op_shl   -> None    (* TODO *)
  | Op_shr   -> None    (* TODO *)
  | Op_shrr  -> None    (* TODO *)
  | Op_add   -> if check_numeric_operand (e1::[e2]) op then None else None     (* TODO *)
  | Op_sub   -> if check_numeric_operand (e1::[e2]) op then None else None     (* TODO *)
  | Op_mul   -> if check_numeric_operand (e1::[e2]) op then None else None     (* TODO *)
  | Op_div   -> if check_numeric_operand (e1::[e2]) op then None else None     (* TODO *)
  | Op_mod   -> if check_numeric_operand (e1::[e2]) op then None else None     (* TODO *)

let rec assign_typing e1 op e2 = match op with    (* TODO *)
  | Assign  -> None    (* TODO *)
  | Ass_add -> None    (* TODO *)
  | Ass_sub -> None    (* TODO *)
  | Ass_mul -> None    (* TODO *)
  | Ass_div -> None    (* TODO *)
  | Ass_mod -> None    (* TODO *)
  | Ass_shl -> None    (* TODO *)
  | Ass_shr -> None    (* TODO *)
  | Ass_shrr-> None    (* TODO *)
  | Ass_and -> None    (* TODO *)
  | Ass_xor -> None    (* TODO *)
  | Ass_or  -> None    (* TODO *)

let rec prefix_typing op e = match op with    (* TODO *)
  | Op_not  -> None    (* TODO *)
  | Op_neg  -> None    (* TODO *)
  | Op_incr -> None    (* TODO *)
  | Op_decr -> None    (* TODO *)
  | Op_bnot -> None    (* TODO *)
  | Op_plus -> None    (* TODO *)

let rec postfix_typing e op = match op with     (* TODO *)
  | Incr    -> None    (* TODO *)
  | Decr    -> None    (* TODO *)

let rec if_typing c e1 e2 = None   (* TODO *)
let rec val_typing v = None        (* TODO *)
let rec array_typing e l = None    (* TODO *)

let rec exp_typing exp classname method_table object_descriptor_table =
  let t=match exp.edesc with
  (*
  | Call of expression option * string * expression list ## 15.12 doc
   *)
  | Call(Some(e),s,l)     -> None    (* TODO *)
  | Call(None,s,l)        -> None    (* TODO *)
  | NewArray(t,l,Some(e)) -> None    (* TODO *)
  | NewArray(t,l,None)    -> None    (* TODO *)
  | ArrayInit(l)          -> None    (* TODO *)
  | Name(id)              -> Some(Ref(Type.mk_type [] id)) (* TODO check *)
  | Attr(o,id)            -> let attr_env = (Env.find object_descriptor_table classname) in
                              if Env.mem attr_env id then let attr = Env.find attr_env id in Some(attr.atype) else Error.unknown_attribute id
  | Array(e,el)           -> None    (* TODO *)
  | New(None,p,_)         -> None    (* TODO *)
  | New(Some o,p,_)       -> None    (* TODO *)
  | Val(v)                -> val_typing v
  | If(c,e1,e2)           -> if_typing c e1 e2
  | CondOp(c,e1,e2)       -> if_typing c e1 e2 (* TODO check if it is the same case than if *)
  | Op(e1,op,e2)          -> infix_typing e1 op e2
  | AssignExp(e1,op,e2)   -> assign_typing e1 op e2
  | Post(e,op)            -> postfix_typing e op
  | Pre(op,e)             -> prefix_typing op e
  | Cast(t,e)             -> begin match e.etype with (* Incomplete *)
                                | Some(t) -> Some(t)
                                | _ -> Error.malformed_expression e end
  | Type(tp)              -> Some(tp)
  | ClassOf(tp)           -> Some(tp)
  | Instanceof(e,t)       -> Some(Primitive(Boolean))
  | VoidClass             -> Some(Void)
  in exp.etype <- t

let execute method_table object_descriptor_table =
  let rec statement_check statement method_ast classname =
   let exp_check exp = exp_typing exp classname method_table object_descriptor_table
   in let iter other = statement_check other method_ast classname
   in let rec list_check l = match l with [] -> () | h::t -> iter h; list_check t;
   in match statement with

    (* To be tested *)
    | Expr(exp)            -> exp_check exp
    | Return(Some(exp))    -> exp_check exp; begin match exp.etype with Some(t) when t = method_ast.mreturntype -> () | _ -> (Error.wrong_return method_ast) end
    | Return(None)         -> begin match method_ast.mreturntype with Void -> () | _ -> (Error.wrong_return method_ast) end 
    (* To be tested - Only check if ref_type is inside the throw block, not that it is actually an exception *)
    | Throw(exp)           -> exp_check exp; begin match exp.etype with Some(Ref(_)) -> () | _ -> (Error.wrong_throw method_ast) end
    
    (* TODO *)
    | Nop                  -> () 
    | Block(sl)            -> list_check sl
    | While(cond,s)        -> iter s
    | If(cond,s1,Some(s2)) -> iter s1; iter s2
    | If(cond,s,None)      -> iter s
    (*  | VarDecl of (Type.t * string * expression option) list *) 
    | VarDecl(l)           -> ()
    (*    | For of (Type.t option * string * expression option) list * expression option * expression list * statement *)
    | For(_,Some(exp),_,s) -> exp_check exp; iter s
    | For(_,None,_,s)      -> iter s
    (*   | Try of statement list * (argument * statement list) list * statement list *)
    | Try(sl1,l,sl2)       -> list_check sl1; list_check sl2
    (* END - TODO *)
  
  in let rec body_check sl method_ast classname = match sl with
    | [] -> ()
    | s::others -> statement_check s method_ast classname; body_check others method_ast classname;
  in let rec method_check ml classname = match ml with
    | [] -> ()
    | m::others -> body_check m.mbody m classname; method_check others classname;
  in let iter_env (id,method_ast) = let classname = List.hd (Str.split_delim (Str.regexp "_") id) in
          body_check method_ast.mbody method_ast classname 
  in Env.iter iter_env method_table;