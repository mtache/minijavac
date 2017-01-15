{
    open Parser
    exception SyntaxError of string

}

let sub = '\x1A'
let space = [' ' '\t']
let identifier = ['a'-'z' 'A'-'Z' '_' '$']['a'-'z' 'A'-'Z' '0'-'9' '_' '$']*
let beginComments = "/*"
let endComments = "*/"
let lineComments = "//"

rule read = parse
    | space+                        { read lexbuf }
    | beginComments { blockComment lexbuf }
    | lineComments { lineComment lexbuf }
    | '\n'                          { Location.incr_line lexbuf; read lexbuf }
    | ';'                           { SEMICOLON }
    | '*'                           { STAR }
    | '.'                           { POINT }
    | ','                           { COMMA }
    | "package"                     { PACKAGE }
    | "import"                      { IMPORT }
    | "static"                      { STATIC }
    | "public"                      { PUBLIC }
    | "protected"                   { PROTECTED }
    | "private"                     { PRIVATE }
    | "abstract"                    { ABSTRACT }
    | "static"                      { STATIC }
    | "final"                       { FINAL }
    | "strictfp"                    { STRICTFP }
    | "class"                       { CLASS }
    | "interface"                   { INTERFACE }
    | "{"                           { RBRACKET }
    | "}"                           { LBRACKET }
    | '('                           { RIGHTP }
    | ')'                           { LEFTP }
    | "extends"                     { EXTENDS }
    | "implements"                  { IMPLEMENTS }
    | "byte"                        { BYTE }
    | "short"                       { SHORT }
    | "int"                         { INT }
    | "long"                        { LONG }
    | "char"                        { CHAR }
    | "float"                       { FLOAT }
    | "double"                      { DOUBLE }
    | "void"                        { VOID }
    | "byte"                        { BYTE }
    | identifier as s               { IDENTIFIER(s) }
    | eof                           { EOF }
    | _                             { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf ^"\n")) }

and blockComment = parse
    | endComments { read lexbuf }
    | _ { blockComment lexbuf }

and lineComment = parse
    | '\n' { read lexbuf }
    | _ { lineComment lexbuf }
