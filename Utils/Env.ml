type ('a,'b) t = ('a, 'b) Hashtbl.t 

let initial () = (Hashtbl.create 17 : ('a,'b) t)

let find env = Hashtbl.find env

let mem env = Hashtbl.mem env

let define env n t = 
  let result = Hashtbl.copy env in
    Hashtbl.add result n t; 
    result

let iter f = Hashtbl.iter (fun s i -> f (s,i))

let print_env_methods env =
  print_string "Methods :\n";
  iter (fun (key,value) ->
  print_string (key^" of type "^(Type.stringOf value))
  ) env
let print_env env =
  iter (fun (key,value) -> 
  print_string ("Class : "^key^"\n");
  print_env_methods value) env