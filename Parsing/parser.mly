%{
    open AST
%}
%token PACKAGE EOF SEMICOLON IMPORT STAR POINT
%token LBRACKET RBRACKET
%token PUBLIC PROTECTED PRIVATE ABSTRACT STATIC FINAL STRICTFP
%token CLASS
%token <string> IDENTIFIER
%type <package_declaration> package_declaration
%type <import_declaration> import_declaration
%type <type_declaration> type_declaration
%type <class_declaration> class_declaration
%start compilation_unit
%type <AST.ast> compilation_unit
%%
compilation_unit:
    | p=package_declaration? i=import_declaration* t=type_declaration* EOF { (p , i, t) }
import_declaration:
    | IMPORT IDENTIFIER SEMICOLON { SingleTypeImport }
    | IMPORT IDENTIFIER POINT STAR SEMICOLON { TypeImportOnDemand }
    | IMPORT STATIC IDENTIFIER POINT IDENTIFIER SEMICOLON { SingleStaticImport }
    | IMPORT STATIC IDENTIFIER POINT STAR  SEMICOLON { StaticImportOnDemand }
package_declaration:
    | PACKAGE p=package_name SEMICOLON { p }
package_name:
    | s=IDENTIFIER { Package(s) }
type_declaration:
    | c=class_declaration { Class(c) }
class_declaration:
    | c=class_modifier* CLASS n=IDENTIFIER RBRACKET LBRACKET { (c, n) }
class_modifier:
    | PUBLIC { Public }
    | PROTECTED { Protected }
    | PRIVATE { Private }
    | ABSTRACT { Abstract }
    | STATIC { Static }
    | FINAL { Final }
    | STRICTFP { Strictfp }