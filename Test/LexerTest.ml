open Parser

let tests_dir = "Test/lexer_files/"

let rec travel_lexbuff lexbuf =
  (* Go through all tokens of the buffer until EOF, raise an exception if the test fail *)
  let token = Lexer.nexttoken lexbuf in
    match token with
      | EOF -> ()
      | _ -> travel_lexbuff lexbuf

let check_lexer file =
  (* Raise an execption if the file can't be interpreted by the lexer *)
  Filereader.read_java file travel_lexbuff
  (* Token2str.print_lexbuf should be replaced by the test function wich compare the lexer output with the expected result *)

let () =
  TestHelper.test_dir tests_dir check_lexer
