{
    open Parser
    exception SyntaxError of string
}

let sub = '\x1A'
let space = [' ' '\t']

rule read = parse
    | space+            { read lexbuf }
    | '\n'              { Location.incr_line lexbuf; read lexbuf }
    | ';'               { SEMICOLON }
    | '*'               { STAR }
    | '.'               { POINT }
    | "package"         { PACKAGE }
    | "import"          { IMPORT }
    | "static"          { STATIC }
    | "public"  { PUBLIC }
    | "protected"   { PROTECTED }
    | "private" { PRIVATE }
    | "abstract"    { ABSTRACT }
    | "static"  { STATIC }
    | "final"   { FINAL }
    | "strictfp"    { STRICTFP }
    | "class"   { CLASS }
    | "interface" { INTERFACE }
    | "{" { RBRACKET }
    | "}" { LBRACKET }
    | "extends" { EXTENDS }
    | "implements" { IMPLEMENTS }
    | ['a'-'z''A'-'Z']+ as s   { IDENTIFIER(s) }
    | eof               { EOF }
    | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }