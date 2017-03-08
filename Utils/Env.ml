type ('a,'b) t = ('a, 'b) Hashtbl.t 

let initial () = (Hashtbl.create 17 : ('a,'b) t)

let find env = Hashtbl.find env

let mem env = Hashtbl.mem env

let define env n t = 
  let result = Hashtbl.copy env in
    Hashtbl.add result n t; 
    result

let replace env n t = 
  let result = Hashtbl.copy env in
    Hashtbl.replace result n t; 
    result

let iter f = Hashtbl.iter (fun s i -> f (s,i))

(* http://pleac.sourceforge.net/pleac_ocaml/hashes.html *)
let assoc_list2hashtbl assoc_list = 
  let h = Hashtbl.create 0 in
  List.iter (fun (k,v) -> Hashtbl.replace h k v) assoc_list ;
  h
let hashtbl2assoc_list h = Hashtbl.fold (fun key value l -> (key, value) :: l) h []

let merge env1 env2 = assoc_list2hashtbl (hashtbl2assoc_list env1 @ hashtbl2assoc_list env2)

let print_class_env env =
  let print_methods_env env =
    print_endline "  Methods :";
    iter (fun (key,value) ->
    print_endline ("    "^key^" of type "^(Type.stringOf value))) env
  in iter (fun (key,value) -> 
  print_endline ("Class : "^key);
  print_methods_env value) env

let print_method_table env =
  iter (fun (key,value) -> 
  print_endline ("Key in table : "^key);
  print_endline ("Name in AST : "^value.AST.mname)) env

(* test : nouvelle fonction pour print object_detector_table (celle d'au dessus ne marche pas) *)
  
let print_class_env_bis env =
  let print_methods_env env =
    print_endline "  Methods :";
    iter (fun (key,value) ->
    print_endline ("    "^key^" of type "^(value.AST.aname))) env
  in iter (fun (key,value) -> 
  print_endline ("Class : "^key);
  print_methods_env value) env