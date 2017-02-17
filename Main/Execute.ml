let execute lexbuf verbose = 
  try 
    let ast = Parser.compilationUnit Lexer.token lexbuf in
    let env = Typing.class_env ast in
    print_endline "successfull parsing";
    if verbose then AST.print_program ast; Env.print_class_env env;
    if Typing.check_class ast then print_endline "successfull check (unimplemented)";
    Compile.init ast;
  with 
    | Parser.Error ->
      print_string "Syntax error: ";
      Location.print (Location.curr lexbuf)
    | Error.Error(e,l) ->
      Error.report_error e;
      Location.print l
