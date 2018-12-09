open Parser

let tests_dir = "Test/blocks_statements_files/"

let rec print_lexbuf lexbuf =
  let exp = block Lexer.nexttoken lexbuf  in
    print_string exp

let check_expression file =
  (* Raise an execption if the file can't be interpreted by the lexer *)
  Filereader.read_java file print_lexbuf
  (* print_lexbuf should be replaced by the test function wich compare the lexer output with the expected result *)

let () =
  TestHelper.test_dir tests_dir check_expression
  