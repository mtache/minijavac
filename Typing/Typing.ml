exception MalformedExpression of string
exception EnvironmentException of string
open AST
open Type
open Env

(*let rec exp_typing exp =
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
*)

let rec class_env ast =
  let rec methods_env = function
    | [] -> Env.initial() 
    | h::t -> let env = methods_env t
              and key = h.mname
              in if Env.mem env key then raise (EnvironmentException("Method "^key^" defined more than once"))
              else Env.define env key h.mreturntype
  in let type_env = function
    | Class(c) -> methods_env c.cmethods
    | Inter -> raise (EnvironmentException("Not implemented"))
  in let tl = ast.type_list
  in let rec type_list_env = function
    | [] -> Env.initial()
    | h::t -> let env = type_list_env t
              and key = h.id
              in if Env.mem env key then raise (EnvironmentException("Class "^key^" defined more than once"))
              else Env.define env key (type_env h.info)
  in type_list_env tl



let rec statement_check s m t env =
   let default = true in
   let rec list_check l = match l with [] -> true | h::u -> (statement_check h m t env) && (list_check u)
   in match s with
  (*  | VarDecl of (Type.t * string * expression option) list *)
    | VarDecl(l)           -> default (* TODO *)
    | Block(sl)            -> list_check sl
    | Nop                  -> default (* TODO *)
    | While(cond,s)        -> default (* TODO *)
(*    | For of (Type.t option * string * expression option) list * expression option * expression list * statement  *)
    | For(w,t,f,_)         -> default (* TODO *)
    | If(cond,s1,Some(s2)) -> default (* TODO *)
    | If(cond,s,None)      -> default (* TODO *)
    | Return(Some(exp))    -> default (* TODO *)
    | Return(None)         -> begin match find (find env t.id) m.mname with Void -> true | _ -> false end
    | Throw(exp)           -> default (* TODO *)
 (*   | Try of statement list * (argument * statement list) list * statement list *)
    | Try(l1,l2,l3)        -> default (* TODO *)
    | Expr(exp)            -> default (* TODO *)



let check_class ast =
  let env = class_env ast
  in let rec body_check sl m t = match sl with
    | [] -> true
    | s::u -> (statement_check s m t env) && (body_check u m t)
  in let rec method_check ml t = match ml with
    | [] -> true
    | m::u -> (body_check m.mbody m t) && (method_check u t)
  in let type_check t = match t.info with
    | Class(c) -> method_check c.cmethods t
    | Inter -> raise (EnvironmentException("Not implemented"))
  in let tl = ast.type_list
  in let rec type_list_check = function
     | [] -> true
     | h::t -> (type_check h) && (type_list_check t)
  in type_list_check tl