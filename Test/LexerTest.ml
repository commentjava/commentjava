open Parser
open Lexer
open Main

let () =
  let file_path = "Test/files/test0.java" in
    let (file, filename) = Main.get_file file_path in
    try
    let input_file = open_in file in
    let lexbuf = Lexing.from_channel input_file in
      Location.init lexbuf file;
      (* print_string lexbuf; *)
      let exp = expression_statement nexttoken lexbuf in
        print_string  exp ;
      close_in (input_file)
    with Sys_error s ->
      print_endline ("Can't find file '" ^ file ^ "'")
