# Typing of statements

Variables : Create one environment for each scope (i.e. Block or For statements) and check if the variables exist.

Return statement : Check whether or not the return statement in a method is consistent with the method signature. If there is no return statement, there is no exceptions raised.

Throw statement : Only check if the expression thrown is a ref_type, not if it is actually an exception.

Try statement : Not implemented.

# Typing of expressions

Since the parser recognizes a value with a point as float and a value without a point as an integer,
we would need to implement different cases when the value is a short, double, byte or long.

What is not implemented :
* Prefix operators
* Postfix operators
* Assign operators besides classic assign operator ("var = some_value")
* New operator syntax for inner classes
* Cast operator
* Arrays

# Running typing phase

Passing Test : `ocamlbuild -use-ocamlfind Main.byte -- Tests/TestsUnitairesTyping.java`

Error Handling Tests : `ocamlbuild -use-ocamlfind Main.byte -- Tests/Error\ Handling\ Tests/*`

# Compilation algorithm

We have changed the compilation algorithm, to make it simpler. It follows these steps :

1. Build the methods table
2. Build the objects descriptors table
3. Check the statements and expressions, and type expressions

# Additional documentation

The file Typing.ml contains some additional documentation, you can easily see what has been implemented and what's been left. We tried to focus on interesting cases (scope, method
existence, attribute existence, etc ...) instead of duplicating similar cases (postfix / prefix, etc ...).

