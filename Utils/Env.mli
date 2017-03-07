(* An environment enables to store and access information associated
   with a string key. *)
type ('a,'b) t

(* creation of a new empty environment *)
val initial : unit -> ('a,'b) t

(* get the value associated to a key *)
val find : ('a,'b) t -> 'a -> 'b

(* is the key associated to a value in the environment *)
val mem : ('a,'b) t -> 'a -> bool

(* define a key with the value associated *)
val define : ('a,'b) t -> 'a -> 'b -> ('a,'b) t

(* iterate a function over all the bindings of the environment *)
val iter : ('a * 'b -> unit) -> ('a,'b) t -> unit

(* Merge two environments *)
val merge : ('a,'b) t -> ('a,'b) t -> ('a,'b) t

(* print function for the class environment *)
val print_class_env : (string, (string, Type.t) t) t -> unit

(* print function for the class environment BIS*)
val print_class_env_bis : (string, (string, AST.astattribute) t) t -> unit

(* print function for the method table *)
val print_method_table : (string, AST.astmethod) t -> unit