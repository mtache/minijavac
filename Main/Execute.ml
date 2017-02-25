let execute lexbuf verbose = 
  try 
    let ast = Parser.compilationUnit Lexer.token lexbuf in
    let (method_table, object_descriptor_table) = Compile.init ast in
    print_endline "successfull parsing";
    if verbose then AST.print_program ast;
    if Typing.check_class ast method_table object_descriptor_table then print_endline "successfull check (unimplemented)";
  with 
    | Parser.Error ->
      print_string "Syntax error: ";
      Location.print (Location.curr lexbuf)
    | Error.Error(e,l) ->
      Error.report_error e;
      Location.print l
