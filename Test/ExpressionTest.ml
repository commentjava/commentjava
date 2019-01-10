open Parser
open Ast

let tests_dir = "Test/expression_files/"

let fail lexbuf checkpoint =
  ErrorHandling.report lexbuf checkpoint;
  assert false

let succeed ast =
  Ast.print_ast ast

let print_lexbuf lexbuf =
  TestHelper.loop lexbuf (Incremental.block_main lexbuf.lex_curr_p) succeed fail

let check_expression file =
  (* Raise an execption if the file can't be interpreted by the lexer *)
  Filereader.read_java file print_lexbuf
  (* print_lexbuf should be replaced by the test function wich compare the lexer output with the expected result *)

let () =
  TestHelper.test_dir tests_dir check_expression
