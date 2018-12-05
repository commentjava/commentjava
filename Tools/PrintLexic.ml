let print_lexic str = 
  Filereader.read_java str Token2str.print_lexbuf

let () =
  Arg.parse [] print_lexic ""