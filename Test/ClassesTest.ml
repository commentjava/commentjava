open Parser
open Ast

let tests_dir = "Test/classes_files/"

let get_nexttoken lexbuf =
    let res = Lexer.nexttoken lexbuf in
    Token2str.print_token res;
    res
;;

let rec print_lexbuf lexbuf =
  let exp = compilation_unit get_nexttoken lexbuf  in
    print_ast exp

let check_expression file =
  (* Raise an execption if the file can't be interpreted by the lexer *)
  Filereader.read_java file print_lexbuf
  (* print_lexbuf should be replaced by the test function wich compare the lexer output with the expected result *)

let () =
  TestHelper.test_dir tests_dir check_expression
