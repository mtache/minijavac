%{
    open AST

%}
%token PACKAGE EOF SEMICOLON IMPORT STAR POINT COMMA
%token LBRACKET RBRACKET LEFTP RIGHTP
%token BYTE SHORT LONG CHAR DOUBLE VOID
%token PUBLIC PROTECTED PRIVATE ABSTRACT STATIC FINAL STRICTFP
%token CLASS INTERFACE EXTENDS IMPLEMENTS


%token  PLUS MINUS DIV TIMES MOD  FALSE TRUE IDFLOAT IDINT IDBYTE IDSHORT IDLONG IDCHAR IDDOUBLE IDBOOLEAN ELSE LPAR RPAR 
/*assignment Operators*/
(* SELFADD SELFSUB SELFMUL SELFDIV SELFAND SELFOR SELFXOR SELFMOD SELFLEFTSHIFT SELFRIGHTSHIFT USELFRIGHTSHIFT *)
%token EQ BREAK
/* statements */
%token  IF FOR WHILE DO  SWITCH  CASE DEFAULT COLON

/*infix operators */
(* %token OR  AND BOR BXOR BAND EQUAL NOTEQUAL LESS GREATER LESSEQUAL GREATEREAQUAL LSHIFT RSHIFT ZFRSHIFT *)

(* %token INSTANCEOF NEW CONTINUE ASSERT QUESTION TRY THEN
%token <string> STRING
%token INCREMENT DECREMENT NEGATION BCOMPLEMENT *)


%token <float> FLOAT
%token <int> INT

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
%type <AST.ast> start
%type < AST.expr > expr
%type < AST.statement > statement
%type  <AST.operation > operation
%type < AST.const > const


%left PLUS MINUS
%left TIMES DIV MOD
%right UMINUS UPLUS

%start start
%%
start:
    | p=package_declaration? i=import_declaration* t=type_declaration* EOF { Formule (p, i, t) }
    | e = expr * EOF { Expression (e)}
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
    | m=class_modifier* CLASS n=IDENTIFIER e=extends_declaration? i=implements_declaration?  LBRACKET RBRACKET  {
        match e with
            | Some([h]) -> { cmodifiers=m; cname=n; cextends=Some(h); cimplements=i }
            | None -> { cmodifiers=m; cname=n; cextends=None; cimplements=i }
            | _ -> raise (DeclarationError ("Multiple extends for the class " ^ n ^ "\n")) }
identifier_list:
    | i=IDENTIFIER { [i] }
    | a=identifier_list COMMA i=IDENTIFIER { a@[i] }
extends_declaration:
    | p=pair(EXTENDS, identifier_list) { match p with (e,i) -> i }
implements_declaration:
    | p=pair(IMPLEMENTS, identifier_list) { match p with (e,i) -> i }
class_modifier:
    | PUBLIC { Public }
    | PROTECTED { Protected }
    | PRIVATE { Private }
    | ABSTRACT { Abstract }
    | STATIC { Static }
    | FINAL { Final }
    | STRICTFP { Strictfp }
interface_declaration:
    | m=class_modifier* INTERFACE n=IDENTIFIER e=extends_declaration? LBRACKET RBRACKET { { imodifiers=m; iname=n; iextends=e } }
name:
    | i=IDENTIFIER { Name([i]) }
    | n=name POINT i=IDENTIFIER { match n with Name(h::t) -> Name(i::h::t) }

/*EXPRESSIONS*/

expr:
	| s=statement 		{ Statement(s)}

  statement:
  | d=declaration {d}
  | i=ifStatement {i}
  | f=forStatement {f}
  | w=whileStatement {w}
  | s=switchStatement {s}
  | o=Operation SEMICOLON {o}

  Operation:
  | o=operation { Operation (o)}

  declaration :
  | i=basicType id=IDENTIFIER EQ o=operation SEMICOLON { Declaration(i,id,Some(o))}
  | i=basicType id=IDENTIFIER SEMICOLON                  { Declaration(i,id, None)}
  | i=basicType id=IDENTIFIER EQ o=operation {Declaration(i,id,Some(o))}


  switchStatement:
  	| SWITCH LPAR op=operation RPAR LBRACKET b=switch_case_group RBRACKET
  		{ SwitchStatement(Switch(op, b))}

  switch_case_group:
  	| s=switch_case {[s]}
  	| s=switch_case b=switch_case_group {s::b}

  switch_case:
  	| CASE op=operation COLON s=statement BREAK SEMICOLON {Normal_case(op,s)}
  	| DEFAULT COLON s=statement 						  {Default_case(s)}


  ifStatement:
    | IF LPAR op=operation RPAR LBRACKET e=statement RBRACKET ELSE LBRACKET e2=statement RBRACKET
              { IfStatement(IfThenElse(op,e,e2)) }
    | IF LPAR op=operation RPAR LBRACKET e=statement RBRACKET
              { IfStatement(IfThen(op,e)) }

  whileStatement:
  | WHILE LPAR op=operation RPAR LBRACKET s = statement RBRACKET { While(op, s) }
  | DO LBRACKET s = statement RBRACKET WHILE LPAR op=operation RPAR  SEMICOLON { DoWhile(op, s) }


  forStatement:
    | FOR LPAR forinit=statement condition=operation SEMICOLON forupdate=statement RPAR LBRACKET action=statement RBRACKET
              { ForStatement(BasicFor(Some(forinit),Some(condition),Some(forupdate),Some(action)))}

  operation:
    | TRUE                                { Bool true}
    | FALSE                               { Bool false}
    | LPAR e=operation RPAR               { e }
    | MINUS e=operation %prec UMINUS      { Unop(Uminus,e)}
    | PLUS e=operation %prec UPLUS        { Unop(Uplus,e)}
    | e1=operation o=bop e2=operation     { Binop(o,e1,e2)}
    | id=IDENTIFIER                       { Var id }
    | c=const                             {Const c}

  const:
    | i=INT {Int i}
    | f=FLOAT {Float f}


  %inline bop:
    | MINUS     { Bsub }
    | PLUS      { Badd }
    | TIMES     { Bmul }
    | DIV       { Bdiv }
    | MOD       { Bmod }

  %inline basicType:
    | IDBYTE    { ByteType }
    | IDSHORT   { ShortType }
    | IDINT     { IntType }
    | IDLONG    { LongType }
    | IDCHAR    { CharType }
    | IDFLOAT   { FloatType }
    | IDDOUBLE   { DoubleType }
    | IDBOOLEAN   { BooleanType }
    
class_body:
    | d=method_declaration* { { cMethods = d } }

method_declaration:
    | h=method_header { { mHeader = h } }

method_header:
    | m=class_modifier* r=jType n=IDENTIFIER LEFTP p=comma_separated_parameters? RIGHTP LBRACKET RBRACKET  { match p with
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

