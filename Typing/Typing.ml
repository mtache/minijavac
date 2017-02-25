open AST
open Type
open Env


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


let rec exp_typing exp =
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
  | Attr(o,id)            -> None    (* TODO *)
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
  in { exp with etype=t }

let rec expr_check exp =
  let default = true in
  match exp.edesc with
  | Call(Some(e),s,l)     -> default (* TODO *)
  | Call(None,s,l)        -> default (* TODO *)
  | NewArray(t,l,Some(e)) -> default (* TODO *)
  | NewArray(t,l,None)    -> default (* TODO *)
  | ArrayInit(l)          -> default (* TODO *)
  | Name(id)              -> default (* TODO *)
  | Attr(o,id)            -> default (* TODO *)
  | Array(e,el)           -> default (* TODO *)
  | New(None,p,_)         -> default (* TODO *)
  | New(Some o,p,_)       -> default (* TODO *)
  | Val(v)                -> default (* TODO *)
  | If(c,e1,e2)           -> default (* TODO *)
  | CondOp(c,e1,e2)       -> default (* TODO *)
  | Op(e1,op,e2)          -> default (* TODO *)
  | AssignExp(e1,op,e2)   -> default (* TODO *)
  | Post(e,op)            -> default (* TODO *)
  | Pre(op,e)             -> default (* TODO *)
  | Cast(t,e)             -> default (* TODO *)
  | Type(t)               -> default (* TODO *)
  | ClassOf(t)            -> default (* TODO *)
  | Instanceof(e,t)       -> default (* TODO *)
  | VoidClass             -> default (* TODO *)


let rec statement_check s m t method_table object_descriptor_table =
   let default = true in
   let rec list_check l = match l with [] -> true | h::u -> (statement_check h m t method_table object_descriptor_table) && (list_check u)
   in match s with
    | Block(sl)            -> list_check sl
    | While(cond,s)        -> (statement_check s m t method_table object_descriptor_table)
    | If(cond,s1,Some(s2)) -> (statement_check s1 m t method_table object_descriptor_table) && (statement_check s2 m t method_table object_descriptor_table)
    | If(cond,s,None)      -> statement_check s m t method_table object_descriptor_table
    | Return(None)         -> default (* begin match find method_table t.id^"_"^m.mname with me when me.mreturntype==Void -> true | _ -> (Error.wrong_return m) end  Tested -> OK *)
    | Expr(exp)            -> expr_check exp

(*    | For of (Type.t option * string * expression option) list * expression option * expression list * statement TODO *)
    | For(_,Some(exp),_,s)         -> (statement_check s m t method_table object_descriptor_table)
    | For(_,None,_,s)         -> (statement_check s m t method_table object_descriptor_table)
 (*   | Try of statement list * (argument * statement list) list * statement list TODO *)
    | Try(sl1,l,sl2)       -> (list_check sl1) && (list_check sl2)
(*  | VarDecl of (Type.t * string * expression option) list TODO *) 
    | VarDecl(l)           -> default (* TODO *)
    | Return(Some(exp))    -> default (* TODO *)
    | Nop                  -> default (* TODO *)  
    | Throw(exp)           -> default (* TODO *)


let check_class ast method_table object_descriptor_table =
  let rec body_check sl m t = match sl with
    | [] -> true
    | s::u -> (statement_check s m t method_table object_descriptor_table) && (body_check u m t)
  in let rec method_check ml t = match ml with
    | [] -> true
    | m::u -> (body_check m.mbody m t) && (method_check u t)
  in let type_check t = match t.info with
    | Class(c) -> method_check c.cmethods t
    | Inter -> Error.not_implemented "Interface cheking" Location.none
  in let tl = ast.type_list
  in let rec type_list_check = function
     | [] -> true
     | h::t -> (type_check h) && (type_list_check t)
  in type_list_check tl