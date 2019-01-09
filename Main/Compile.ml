(* verbose is a boolean that you can use to switch to a verbose output (for example, to dump all the ast) *)
let execute lexbuf verbose = 
  let ast = Parser.compilation_unit Lexer.nexttoken lexbuf in
  begin match verbose with
    | true -> Ast.print_compilationUnit ast 0
    | false -> ()
  end;
  print_endline "";
  print_endline "done compiling";;
