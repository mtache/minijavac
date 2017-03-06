open AST
open Type

let get_exp_type exp = match exp.etype with None -> Error.unknown_type exp | Some(t) -> t

let rec check_integral_operand expl op = let check exp = match exp.etype with
  | Some(Primitive(Boolean)) -> Error.non_integral_operand exp op
  | Some(Primitive(Char)) -> Error.non_integral_operand exp op
  | Some(Primitive(Float)) -> Error.non_integral_operand exp op
  | Some(Primitive(Double)) -> Error.non_integral_operand exp op
  | Some(Primitive(_)) -> ()
  | _ -> Error.non_integral_operand exp op
  in match expl with [] -> () | h::t -> check h; check_integral_operand t op

let rec check_numeric_operand expl op = let check exp = match exp.etype with
  | Some(Primitive(Boolean)) -> Error.non_numeric_operand exp op
  | Some(Primitive(Char)) -> Error.non_numeric_operand exp op
  | Some(Primitive(_)) -> ()
  | _ -> Error.non_numeric_operand exp op
  in match expl with [] -> () | h::t -> check h; check_numeric_operand t op

let rec check_boolean_operand expl op = let check exp = match exp.etype with
  | Some(Primitive(Boolean)) -> ()
  | _ -> Error.non_boolean_operand exp op
  in match expl with [] -> () | h::t -> check h; check_boolean_operand t op

let infix_numeric_typing e1 op e2 = check_numeric_operand (e1::[e2]) op; match e1.etype with
   | t when t = e2.etype -> t
   | t when t = Some(Primitive(Byte)) -> e2.etype
   | t when t = Some(Primitive(Double)) -> t
   | t1 when t1 = Some(Primitive(Short))  -> begin match e2.etype with
                                            | t2 when t2 = Some(Primitive(Float))  -> t2
                                            | t2 when t2 = Some(Primitive(Double)) -> t2
                                            | t2 when t2 = Some(Primitive(Long))   -> t2
                                            | t2 when t2 = Some(Primitive(Int))   -> t2
                                            | _ -> t1 end
   | t1 when t1 = Some(Primitive(Int)) -> begin match e2.etype with
                                            | t2 when t2 = Some(Primitive(Float))  -> t2
                                            | t2 when t2 = Some(Primitive(Double)) -> t2
                                            | t2 when t2 = Some(Primitive(Long))   -> t2
                                            | _ -> t1 end
   | t1 when t1 = Some(Primitive(Long)) -> begin match e2.etype with
                                            | t2 when t2 = Some(Primitive(Float))  -> t2
                                            | t2 when t2 = Some(Primitive(Double)) -> t2
                                            | _ -> t1 end
   | t1 when t1 = Some(Primitive(Float))  -> begin match e2.etype with
                                            | t2 when t2 = Some(Primitive(Double)) -> t2
                                            | _ -> t1 end
                                                                                 
let rec infix_typing e1 op e2 = match op with    (* TODO *)
  (* 15.17 Multiplicative Operators 491 *)  (* To be tested *)
  | Op_mul   -> infix_numeric_typing e1 op e2
  | Op_div   -> infix_numeric_typing e1 op e2
  | Op_mod   -> infix_numeric_typing e1 op e2
  (* 15.18 Additive Operators 496 *)  (* To be tested *)
  | Op_add   -> infix_numeric_typing e1 op e2 (* TODO - Handle when operands are strings *)
  (* if ((check_numeric_operand (e1::[e2]) op) && (e1.etype = e2.etype)) then e1.etype else Error.invalid_operand e1 op e2 e1.eloc *)
  | Op_sub   -> infix_numeric_typing e1 op e2
  (* 15.19 Shift Operators 502 *) (* To be tested *)
  | Op_shl   -> check_integral_operand (e1::[e2]) op; e1.etype;
  | Op_shr   -> check_integral_operand (e1::[e2]) op; e1.etype;
  | Op_shrr  -> check_integral_operand (e1::[e2]) op; e1.etype;
  (* 15.20 Relational Operators 503 *) (* To be tested *)
  | Op_gt    -> check_numeric_operand (e1::[e2]) op; Some(Primitive(Boolean))
  | Op_lt    -> check_numeric_operand (e1::[e2]) op; Some(Primitive(Boolean))
  | Op_ge    -> check_numeric_operand (e1::[e2]) op; Some(Primitive(Boolean))
  | Op_le    -> check_numeric_operand (e1::[e2]) op; Some(Primitive(Boolean))
  (* 15.21 Equality Operators 505 *) (* To be tested *)
  | Op_eq    -> begin match e1.etype, e2.etype with
                  | Some(Primitive(Boolean)), Some(Primitive(Boolean)) -> Some(Primitive(Boolean))
                  | Some(Ref(_)), Some(Ref(_)) -> Some(Primitive(Boolean))
                  | Some(Ref(_)), Some(Void) -> Some(Primitive(Boolean))
                  | Some(Void), Some(Ref(_)) -> Some(Primitive(Boolean))
                  | Some(Primitive(Char)), Some(Primitive(Char)) -> Some(Primitive(Boolean))
                  | _, _ -> check_numeric_operand (e1::[e2]) op; Some(Primitive(Boolean)) end
  | Op_ne    -> begin match e1.etype, e2.etype with
                  | Some(Primitive(Boolean)), Some(Primitive(Boolean)) -> Some(Primitive(Boolean))
                  | Some(Ref(_)), Some(Ref(_)) -> Some(Primitive(Boolean))
                  | Some(Ref(_)), Some(Void) -> Some(Primitive(Boolean))
                  | Some(Void), Some(Ref(_)) -> Some(Primitive(Boolean))
                  | Some(Primitive(Char)), Some(Primitive(Char)) -> Some(Primitive(Boolean))
                  | _, _ -> check_numeric_operand (e1::[e2]) op; Some(Primitive(Boolean)) end
  (* 15.22 Bitwise and Logical Operators 508 *)
  | Op_or    -> None    (* TODO *)
  | Op_xor   -> None    (* TODO *)
  | Op_and   -> None    (* TODO *)
  (* 15.23 Conditional-And Operator && 509 *)
  | Op_cand  -> check_boolean_operand (e1::[e2]) op; Some(Primitive(Boolean)) 
  (* 15.24 Conditional-Or Operator || 509 *)
  | Op_cor   -> check_boolean_operand (e1::[e2]) op; Some(Primitive(Boolean))  

let rec assign_typing e1 op e2 = match op with    (* TODO *)
  | Assign  -> if (e1.etype = e2.etype) then e1.etype else Error.assign_incompatible_types (get_exp_type e1) e2
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

let rec array_typing e l = None    (* TODO *)

let rec val_typing v = match v with
  | String(value) -> None (* TODO special class, not an actual primitive *)
  | Int(value) -> Some(Primitive(Int))
  | Float(value) -> Some(Primitive(Float))
  | Char(Some(value)) -> Some(Primitive(Char))
  | Char(None) -> Some(Primitive(Char))
  | Null -> None (* TODO special value, not an actual primitive *)
  | Boolean(value) -> Some(Primitive(Boolean))

let rec exp_typing exp classname method_table object_descriptor_table var_env =
  let rec iter exp = exp_typing exp classname method_table object_descriptor_table var_env in
  let t=match exp.edesc with
  (*
  | Call of expression option * string * expression list ## 15.12 doc
   *)
  | Call(Some(e),id,params) -> iter e; let exp_type = get_exp_type e in if Env.mem method_table ((Type.stringOf exp_type)^"_"^id) then let meth = Env.find method_table ((Type.stringOf exp_type)^"_"^id) in Some(meth.mreturntype) else Error.unknown_method id exp.eloc
  | Call(None,id,params)  -> if Env.mem method_table (classname^"_"^id) then let meth = Env.find method_table (classname^"_"^id) in Some(meth.mreturntype) else Error.unknown_method id exp.eloc
  | NewArray(t,l,Some(e)) -> None    (* TODO *)
  | NewArray(t,l,None)    -> None    (* TODO *)
  | ArrayInit(l)          -> None    (* TODO *)
  | Name(id)              -> if Hashtbl.mem var_env id then let var_type, exp = Hashtbl.find var_env id in Some(var_type) else Error.unknown_variable id exp.eloc
  | Attr(o,id)            -> let attr_env = (Env.find object_descriptor_table classname) in
                              if Env.mem attr_env id then let attr = Env.find attr_env id in Some(attr.atype) else Error.unknown_attribute id exp.eloc
  | Array(e,el)           -> None    (* TODO *)
  | New(None,p,_)         -> if Env.mem object_descriptor_table (String.concat "." p) then Some(Ref(Type.extract_type p)) else Error.unknown_class (String.concat "." p) exp.eloc
  | New(Some o,p,_)       -> None    (* Not implemented syntax for inner classes ... *)
  | Val(v)                -> val_typing v
  | If(c,e1,e2)           -> iter e1; iter e2; if_typing c e1 e2
  | CondOp(c,e1,e2)       -> iter e1; iter e2; if_typing c e1 e2 (* TODO check if it is the same case than if *)
  | Op(e1,op,e2)          -> iter e1; iter e2; infix_typing e1 op e2 
  | AssignExp(e1,op,e2)   -> iter e1; iter e2; assign_typing e1 op e2 (* TODO *)
  | Post(e,op)            -> iter e; postfix_typing e op (* TODO *)
  | Pre(op,e)             -> iter e; prefix_typing op e (* TODO *)
  | Cast(t,e)             -> iter e; begin match e.etype with (* Incomplete *)
                                | Some(t) -> Some(t)
                                | _ -> Error.malformed_expression e end
  | Type(tp)              -> Some(tp)
  | ClassOf(tp)           -> Some(tp)
  | Instanceof(e,t)       -> iter e; Some(Primitive(Boolean))
  | VoidClass             -> Some(Void)
  in exp.etype <- t

let execute method_table object_descriptor_table =
  let rec statement_check statement method_ast classname var_env =
   let exp_check exp env = exp_typing exp classname method_table object_descriptor_table env
   and iter other env = statement_check other method_ast classname env
   in let rec list_check s_list env = match s_list with [] -> () | h::t -> statement_check h method_ast classname env; list_check t env
   and add_var var_list env = match var_list with
                                  | [] -> ()
                                  | (var_type, id, None)::t -> Hashtbl.add env id (var_type,None); add_var t env
                                  | (var_type, id, Some(exp))::t -> Hashtbl.add env id (var_type,Some(exp)); add_var t env; exp_check exp env; if (get_exp_type exp = var_type) then () else Error.assign_incompatible_types var_type exp
   and add_var_for var_list env = match var_list with
                                  | [] -> ()
                                  | (Some(var_type), id, None)::t -> Hashtbl.add env id (var_type,None); add_var_for t env
                                  | (Some(var_type), id, Some(exp))::t -> exp_check exp env; Hashtbl.add env id (var_type,Some(exp)); add_var_for t env
                                  | (None, id, None)::t -> if Hashtbl.mem env id then () else Error.unknown_variable id method_ast.mloc
                                  | (None, id, Some(exp))::t -> if Hashtbl.mem env id then begin let var_type, _ = Hashtbl.find   env id in exp_check exp env; Hashtbl.remove env id; Hashtbl.add env id (var_type,Some(exp)) end else Error.unknown_variable id method_ast.mloc
   and exp_list_check exp_list env = match exp_list with [] -> () | h::t -> exp_check h env; exp_list_check t env
   in match statement with
    (* All good *)
    | Nop                  -> ()
    | Expr(exp)            -> exp_check exp var_env
    | Block(sl)            -> list_check sl (Hashtbl.copy var_env) (* New block = new scope for variables ! *)
    | VarDecl(var)         -> add_var var var_env
    | Return(Some(exp))    -> exp_check exp var_env; begin match exp.etype with Some(t) when t = method_ast.mreturntype -> () | _ -> (Error.wrong_return method_ast) end
    | Return(None)         -> begin match method_ast.mreturntype with Void -> () | _ -> (Error.wrong_return method_ast) end 
    (* Throw - Only check if ref_type is inside the throw block, not that it is actually an exception *)
    | Throw(exp)           -> exp_check exp var_env; begin match exp.etype with Some(Ref(_)) -> () | _ -> (Error.wrong_throw method_ast) end
    | While(cond,s)        -> exp_check cond var_env; iter s var_env
    | If(cond,s1,Some(s2)) -> exp_check cond var_env; iter s1 var_env; iter s2 var_env
    | If(cond,s,None)      -> exp_check cond var_env; iter s var_env
    | For(var,Some(cond),exp_list,s) -> let for_env = (Hashtbl.copy var_env) in add_var_for var for_env; exp_check cond for_env; exp_list_check exp_list for_env; iter s for_env
    | For(var,None,exp_list,s)       -> let for_env = (Hashtbl.copy var_env) in add_var_for var for_env; exp_list_check exp_list for_env; iter s for_env
    (* END - All good *)
    
    (* TODO *)
    (*   | Try of statement list * (argument * statement list) list * statement list *)
    | Try(sl1,l,sl2)       -> list_check sl1 (Hashtbl.copy var_env); list_check sl2 (Hashtbl.copy var_env)
    (* END - TODO *)
  
  in let rec body_check sl method_ast classname var_env = match sl with
    | [] -> ()
    | s::others -> statement_check s method_ast classname var_env; body_check others method_ast classname var_env
  in let iter_env (id,method_ast) = let classname = List.hd (Str.split_delim (Str.regexp "_") id) in
          body_check method_ast.mbody method_ast classname (Hashtbl.create 4 : (string, Type.t * AST.expression option) Hashtbl.t)
  in Env.iter iter_env method_table;