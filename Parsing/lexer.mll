{
    open Parser
    exception SyntaxError of string
}

let sub = '\x1A'
let space = [' ' '\t' '\n']

rule read = parse
    | space+            { read lexbuf }
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
    | "{" { RBRACKET }
    | "}" { LBRACKET }
    | ['a'-'z']+ as s   { IDENTIFIER(s) }
    | eof     { EOF }
    | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }