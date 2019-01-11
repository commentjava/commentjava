(* verbose is a boolean that you can use to switch to a verbose output (for example, to dump all the ast) *)

module I =
  Parser.MenhirInterpreter

let fail lexbuf checkpoint =
  ErrorHandling.report lexbuf checkpoint

let succeed_verbose (v : Ast.compilationUnit) =
  Ast.print_compilationUnit v 0

let succeed (v : Ast.compilationUnit) =
  ()

let loop lexbuf result succeed =
  let supplier = I.lexer_lexbuf_to_supplier Lexer.nexttoken lexbuf in
  I.loop_handle succeed (fail lexbuf) supplier result

let execute lexbuf verbose = 
  begin match verbose with
    | true -> loop lexbuf (Parser.Incremental.compilation_unit lexbuf.lex_curr_p) succeed_verbose;
    | false -> loop lexbuf (Parser.Incremental.compilation_unit lexbuf.lex_curr_p) succeed;
  end;
  print_endline "";
  print_endline "done compiling";;
