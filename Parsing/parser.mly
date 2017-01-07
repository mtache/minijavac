%{
    open AST
%}
%token PACKAGE EOF SEMICOLON
%token <string> IDENTIFIER
%start compilation_unit
%type <AST.ast> compilation_unit
%%
compilation_unit:
    | p=package_declaration EOF { Some(p) }
package_declaration:
    | PACKAGE p=package_name SEMICOLON { p }
package_name:
    | s=IDENTIFIER { Package(s) }