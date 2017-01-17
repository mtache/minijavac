

let execute lexbuf verbose = 
  try 
    let ast = Parser.compilationUnit Lexer.token lexbuf in
    let env = Typing.env_class ast in
    print_endline "successfull parsing";
    if verbose then AST.print_program ast; Env.print_env env;
  with 
    | Parser.Error ->
      print_string "Syntax error: ";
      Location.print (Location.curr lexbuf)
    | Error.Error(e,l) ->
      Error.report_error e;
      Location.print l
