%{
    open AST
%}
%token PACKAGE EOF SEMICOLON IMPORT STAR POINT
%token LBRACKET RBRACKET
%token PUBLIC PROTECTED PRIVATE ABSTRACT STATIC FINAL STRICTFP
%token CLASS INTERFACE
%token <string> IDENTIFIER
%type <package> package_declaration
%type <import> import_declaration
%type <type_declaration> type_declaration
%type <class_declaration> class_declaration
%type <interface_declaration> interface_declaration
%type <qualified_name> qualified_name
%start start
%type <AST.ast> start
%%
start:
    | p=package_declaration? i=import_declaration* t=type_declaration* EOF { (p, i, t) }
import_declaration:
    | IMPORT t=type_name SEMICOLON { SingleTypeImport(t) }
    | IMPORT t=type_name POINT STAR SEMICOLON { TypeImportOnDemand(t) } (*TODO package or type names *)
    | IMPORT STATIC t=type_name POINT IDENTIFIER SEMICOLON { SingleStaticImport(t) }
    | IMPORT STATIC t=type_name POINT STAR SEMICOLON { StaticImportOnDemand(t) }
package_declaration:
    | PACKAGE p=qualified_name SEMICOLON { Package(p) }
type_declaration:
    | c=class_declaration { Class(c) }
    | i=interface_declaration { Interface(i) }
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
interface_declaration:
    | i=interface_modifier* INTERFACE n=IDENTIFIER RBRACKET LBRACKET { (i, n) }
interface_modifier:
    | PUBLIC { Public }
    | PROTECTED { Protected }
    | PRIVATE { Private }
    | ABSTRACT { Abstract }
    | STATIC { Static }
    | STRICTFP { Strictfp }
qualified_name:
    | n=separated_nonempty_list(POINT, IDENTIFIER) { n }
type_name: (* FIXME probably duplicate with qualified_name *)
    | i=IDENTIFIER { i }
    | t=type_name POINT i=IDENTIFIER { t^"."^i }
