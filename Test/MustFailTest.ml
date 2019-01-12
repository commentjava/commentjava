open Parser
open Ast

let tests_dir = "Test/must_fail_files/"

let fail lexbuf checkpoint = ()
  (* ErrorHandling.report lexbuf checkpoint; *)

let succeed ast =
  Ast.print_compilationUnit ast 0;
  print_newline ();
  assert false

let print_lexbuf lexbuf =
    TestHelper.loop lexbuf (Parser.Incremental.compilation_unit lexbuf.lex_curr_p) succeed fail

let check_file file =
  (* Raise an execption if the file can't be interpreted by the lexer *)
  Filereader.read_java file print_lexbuf
  (* print_lexbuf should be replaced by the test function wich compare the lexer output with the expected result *)

let () =
  TestHelper.test_dir tests_dir check_file
