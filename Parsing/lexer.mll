{
    open Parser
    exception SyntaxError of string
}

let sub = '\x1A'
let space = [' ' '\t' '\n']

rule read = parse
    | space+  { read lexbuf }
    | ';'       { SEMICOLON }
    | "package" { PACKAGE }
    | ['a'-'z']+ as s   { IDENTIFIER(s) }
    | eof     { EOF }
    | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }