%{
    open AST
%}
%token PACKAGE EOF SEMICOLON IMPORT STATIC STAR POINT
%token <string> IDENTIFIER
%type <package_declaration> package_declaration
%type <import_declaration> import_declaration
%start start
%type <AST.ast> start
%%
start:
    | p=package_declaration? i=import_declaration* EOF { (p , i) }
import_declaration:
    | IMPORT IDENTIFIER SEMICOLON { SingleTypeImport }
    | IMPORT IDENTIFIER POINT STAR SEMICOLON { TypeImportOnDemand }
    | IMPORT STATIC IDENTIFIER POINT IDENTIFIER SEMICOLON { SingleStaticImport }
    | IMPORT STATIC IDENTIFIER POINT STAR  SEMICOLON { StaticImportOnDemand }
package_declaration:
    | PACKAGE p=package_name SEMICOLON { p }
package_name:
    | s=IDENTIFIER { Package(s) }