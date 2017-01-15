%{
    open AST
%}
%token PACKAGE EOF SEMICOLON IMPORT STAR POINT
%token LBRACKET RBRACKET
%token PUBLIC PROTECTED PRIVATE ABSTRACT STATIC FINAL STRICTFP
%token CLASS INTERFACE EXTENDS IMPLEMENTS
%token <string> IDENTIFIER
%type <package> package_declaration
%type <import> import_declaration
%type <classnode> class_declaration
%type <modifier> class_modifier
%type <name> name
%start start
%type <AST.ast> start
%%
start:
    | p=package_declaration? i=import_declaration* t=type_declaration* EOF { (p, i, t) }

(* Import declarations *)
import_declaration:
    | IMPORT s=STATIC? i=import_name SEMICOLON { let (name, ondemand)=i in match (s, ondemand) with
                                                                | (Some(), false) -> SingleStaticImport(name)
                                                                | (None, true) -> TypeImportOnDemand(name)
                                                                | (Some(), true) -> StaticImportOnDemand(name)
                                                                | (None, false) -> SingleTypeImport(name) }
import_name:
    | i=terminated(terminated(name, POINT), STAR) { (i,true) }
    | i=name { (i,false) }
(* Package declaration *)
package_declaration:
    | PACKAGE p=name SEMICOLON { Package(p) }
(* Type declarations *)
type_declaration:
    | c=class_declaration { Class(c) }
    | i=interface_declaration { Interface(i) }
class_declaration:
    | m=class_modifier* CLASS n=IDENTIFIER e=extends_declaration? i=implements_declaration? RBRACKET LBRACKET { { cmodifiers=m; cname=n; cextends=e; cimplements=i } }
extends_declaration:
    | p=pair(EXTENDS, IDENTIFIER) { match p with 
                                        | (e,i) -> i }
implements_declaration:
    | p=pair(IMPLEMENTS, IDENTIFIER) { match p with 
                                        | (e,i) -> i }
class_modifier:
    | PUBLIC { Public }
    | PROTECTED { Protected }
    | PRIVATE { Private }
    | ABSTRACT { Abstract }
    | STATIC { Static }
    | FINAL { Final }
    | STRICTFP { Strictfp }
interface_declaration:
    | m=class_modifier* INTERFACE n=IDENTIFIER e=extends_declaration? RBRACKET LBRACKET { { imodifiers=m; iname=n; iextends=e } }
name:
    | i=IDENTIFIER { Name([i]) }
    | n=name POINT i=IDENTIFIER { match n with Name(h::t) -> Name(i::h::t) }