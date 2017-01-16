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
let digit = ['0'-'9']
let intiger = digit+
let floating = digit+ '.' digit*


rule read = parse
    | space+                        { read lexbuf }
    | beginComments { blockComment lexbuf }
    | lineComments { lineComment lexbuf }

    | intiger as nb {INT (int_of_string (nb))}
    | floating as nb   { FLOAT (float_of_string nb)}

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
    | "do"                          {DO}


    | "if"          { IF }
    | "else"        { ELSE }

    | "for"         { FOR }
    | "while"       { WHILE }

    | "switch"      { SWITCH }
    | "case"        { CASE }
    | "default"     { DEFAULT }
    | "break"       { BREAK}
    | "false"       { FALSE }
    | "true"        { TRUE }
    | "float"       { IDFLOAT }
    | "int"         { IDINT }
    | "byte"        { IDBYTE }
    | "short"       { IDSHORT }
    | "long"        { IDLONG }
    | "char"        { IDCHAR }
    | "double"      { IDDOUBLE }
    | "boolean"     { IDBOOLEAN }
    | "final"                       { FINAL }
    | "strictfp"                    { STRICTFP }
    | "class"                       { CLASS }
    | "interface"                   { INTERFACE }
    | "{"                           { LBRACKET }
    | "}"                           { RBRACKET }
    | "extends"                     { EXTENDS }
    | "implements"                  { IMPLEMENTS }
    
    | "+"           { PLUS }
    | "-"           { MINUS }
    | "/"           { DIV }
    | "*"           { TIMES }
    | "%"           { MOD }
    | ":"           { COLON }

    | "="           {EQ}




    | "("           { LPAR } (* Duplicated *)
    | ")"           { RPAR } (* Duplicated *)

    | '('                           { RIGHTP } (* Duplicated *)
    | ')'                           { LEFTP } (* Duplicated *)
    | "byte"                        { IDBYTE } 
    | "short"                       { SHORT }
    | "int"                         { IDINT }
    | "long"                        { IDLONG }
    | "char"                        { IDCHAR }
    | "float"                       { IDFLOAT }
    | "double"                      { IDDOUBLE }
    | "void"                        { VOID }
    | "byte"                        { IDBYTE }
    
    | identifier as s               { IDENTIFIER(s) }

    | eof                           { EOF }
    | _                             { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf ^"\n")) }

and blockComment = parse
    | endComments { read lexbuf }
    | _ { blockComment lexbuf }

and lineComment = parse
    | '\n' { read lexbuf }
    | eof  { EOF }
    | _ { lineComment lexbuf }
