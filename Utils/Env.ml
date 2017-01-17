type ('a,'b) t = ('a, 'b) Hashtbl.t 

let initial () = (Hashtbl.create 17 : ('a,'b) t)

let find env = Hashtbl.find env

let mem env = Hashtbl.mem env

let define env n t = 
  let result = Hashtbl.copy env in
    Hashtbl.add result n t; 
    result

let iter f = Hashtbl.iter (fun s i -> f (s,i))

let print_class_env env =
  let print_methods_env env =
    print_endline "  Methods :";
    iter (fun (key,value) ->
    print_endline ("    "^key^" of type "^(Type.stringOf value))) env
  in iter (fun (key,value) -> 
  print_endline ("Class : "^key);
  print_methods_env value) env