%{
    open AST

%}
%token PACKAGE EOF SEMICOLON IMPORT STAR POINT COMMA
%token LBRACKET RBRACKET LEFTP RIGHTP
%token BYTE SHORT INT LONG CHAR FLOAT DOUBLE VOID
%token PUBLIC PROTECTED PRIVATE ABSTRACT STATIC FINAL STRICTFP
%token CLASS INTERFACE EXTENDS IMPLEMENTS
%token <string> IDENTIFIER
%type <package> package_declaration
%type <import> import_declaration
%type <classnode> class_declaration
%type <modifier> class_modifier
%type <name> name
%type <classBody> class_body
%type <methodHeader> method_header
%type <methodDeclaration> method_declaration
%type <parameters> parameters
%type <jType> jType
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
    | m=class_modifier* CLASS n=IDENTIFIER e=extends_declaration? i=implements_declaration? RBRACKET b=class_body LBRACKET {
        match e with
            | Some([h]) -> { cmodifiers=m; cname=n; cextends=Some(h); cimplements=i }
            | None -> { cmodifiers=m; cname=n; cextends=None; cimplements=i }
            | _ -> raise (DeclarationError ("Multiple extends for the class " ^ n ^ "\n")) }
identifier_list:
    | i=IDENTIFIER { [i] }
    | a=identifier_list COMMA i=IDENTIFIER { a@[i] }
extends_declaration:
    | p=pair(EXTENDS, identifier_list) { match p with 
                                        | (e,i) -> i }
implements_declaration:
    | p=pair(IMPLEMENTS, identifier_list) { match p with 
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
    | m=class_modifier* INTERFACE n=IDENTIFIER e=extends_declaration? RBRACKET class_body LBRACKET { { imodifiers=m; iname=n; iextends=e } }
name:
    | i=IDENTIFIER { Name([i]) }
    | n=name POINT i=IDENTIFIER { match n with Name(h::t) -> Name(i::h::t) }

class_body:
    | d=method_declaration* { { cMethods = d } }

method_declaration:
    | h=method_header { { mHeader = h } }

method_header:
    | m=class_modifier* r=jType n=IDENTIFIER RIGHTP p=comma_separated_parameters? LEFTP RBRACKET LBRACKET { match p with
                                                                                                                | (Some(param)) -> { mModifier = m; mResultType = r; mName = n; mParameters = param }
                                                                                                                | _ -> { mModifier = m; mResultType = r; mName = n; mParameters = [] } }

jType:
    | BYTE { Byte }
    | SHORT { Short }
    | INT { Int }
    | LONG { Long }
    | CHAR { Char }
    | FLOAT { Float }
    | DOUBLE { Double }
    | VOID { Void }

parameters:
    | t=jType i=IDENTIFIER { { pType=t; pName=i} }

comma_separated_parameters:
    | p=parameters { [p] }
    | c=comma_separated_parameters COMMA p=parameters { match c with h::t -> p::h::t }