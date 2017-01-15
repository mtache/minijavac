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

    | "final"                       { FINAL }
    | "strictfp"                    { STRICTFP }
    | "class"                       { CLASS }
    | "interface"                   { INTERFACE }
    | "{"                           { RBRACKET }
    | "}"                           { LBRACKET }
    | "extends"                     { EXTENDS }
    | "implements"                  { IMPLEMENTS }
    | identifier as s               { IDENTIFIER(s) }

    | ";"           {SEMICOLON}
    | "+"           { PLUS }
    | "-"           { MINUS }
    | "/"           { DIV }
    | "*"           { TIMES }
    | "%"           { MOD }
    | ";"           { SEMICOLON }
    | ":"           { COLON }
    | "?"           { QUESTION }

    | "="           {EQ}
    | "+="          {SELFADD}
    | "-="          {SELFSUB}
    | "*="          {SELFMUL}
    | "/="          {SELFDIV}
    | "&="          {SELFAND}
    | "|="          {SELFOR}
    | "^="          {SELFXOR}
    | "%="          {SELFMOD}
    | "<<="         {SELFLEFTSHIFT}
    | ">>="         {SELFRIGHTSHIFT}
    | ">>>="        {USELFRIGHTSHIFT}


    | "||"  {OR}
    | "&&"  {AND}
    | "|"  {BOR}
    | "^"  {BXOR}
    | "&"  {BAND}
    | "=="  {EQUAL}
    | "!="  {NOTEQUAL}
    | "<"  {LESS}
    | ">"  {GREATER}
    | "<="  {LESSEQUAL}
    | ">="  {GREATEREAQUAL}
    | "<<"  {LSHIFT}
    | ">>"  {RSHIFT}
    | ">>>"  {ZFRSHIFT}

    | "++"        {INCREMENT}
    | "--"        {DECREMENT}
    | "!"       {NEGATION}
    | "~"       {BCOMPLEMENT}


    | eof                           { EOF }
    | _                             { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf ^"\n")) }

and blockComment = parse
    | endComments { read lexbuf }
    | _ { blockComment lexbuf }

and lineComment = parse
    | '\n' { read lexbuf }
    | _ { lineComment lexbuf }