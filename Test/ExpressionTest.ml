open Parser
open Lexing

let successCount = ref 0
let failCount = ref 0

let to_test = [
  "Test/expression_files/test0.java"
]

let rec print_lexbuf lexbuf =
  let exp = expression_statement Lexer.nexttoken lexbuf  in
    print_string exp

(* Token2str.print_lexbuf should be replaced by the test function wich compare the lexer output with the expected result *)
let tester str = 
  try
    print_endline ("## Start Expression test for " ^ str);
    Filereader.read_java str print_lexbuf;
    print_endline ("\n## End of Expression test for " ^ str ^ "\n\n");
    successCount := !successCount + 1
  with
    _ -> failCount := !failCount + 1; print_endline ("/!\\/!\\ Error while testing " ^ str ^ " /!\\/!\\\n\n")

let () =
  List.iter tester to_test;
  print_endline ("### Success: " ^ (string_of_int !successCount));
  print_endline ("### Failed: " ^ (string_of_int !failCount))
