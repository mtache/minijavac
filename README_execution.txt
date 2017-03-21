README - Execution part

Bartosz Golrewicz - Théophile Debauche

We were based on the method_table, the object_desciptor_table, and the typed AST, provided by the Typing part.

To simplify, we just executed the Main Class of the Java programm, and we didn't try to interact with other class or methods.

We used a very simple model of memory : a double degree HashTable (defined in the Env.ml from the Utils directory). A state of the memory looks like the following :

Type : float
 Variable :
    z of value 9.6
    f of value 2.
Type : int
 Variable :
    n of value 11
    e of value 7
Type : boolean
 Variable :
    a of value false
    p of value false
    b of value true

The first level contains (key: type, value : second level)
The second level contains (key : name of variable, value : value of variable)

Now that we have the memory, we implemented a few functions to get / add / modify / etc an element in the memory.

The main algorithm consists in executing the statements of the Main, one by one. We implemented the following statements :
- Variable declaration
- Expression execution (mostly assignments)
- If
- While
- Block
- For

About the eexpressions, we are able to calculate many things, numbers and boolean as well. 

We simplified the basic operations by considering almost every numeric variable as a float, to go faster to the most interesting parts. We are thus able to execute basic algorithms using the statements listed before.

To run the execution, type :
ocamlbuild Main.byte -- Tests/TestsUnitairesExec.java
on a terminal.

