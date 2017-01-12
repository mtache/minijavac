(* verbose is a boolean that you can use to switch to a verbose output (for example, to dump all the ast) *)
open Parser
open Lexer
open Location
open Lexing
open AST

let parse_with_error lexbuf =
  try Parser.start Lexer.read lexbuf with
  | SyntaxError msg ->
    print_string (msg);
    Location.print (Location.curr lexbuf);
    exit (-1)
  | Parser.Error ->
    print_string ("Parser error\n");
    Location.print (Location.curr lexbuf);
    exit (-1)
  | Failure msg ->
    print_string (msg);
    Location.print (Location.curr lexbuf);
    exit (-1)


let execute lexbuf verbose = 
    let ast = parse_with_error lexbuf in
    print_ast ast;
    print_string "\n"