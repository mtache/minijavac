%{
%}
%token PACKAGE EOF SEMICOLON
%token <string> IDENTIFIER
%start compilation_unit
%type <unit> compilation_unit
%%
compilation_unit:
    | p=package_declaration EOF { p }
package_declaration:
    | PACKAGE p=package_name SEMICOLON { p }
package_name:
    | s=IDENTIFIER { print_string s }