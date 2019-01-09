let verbose = ref false

let compile str = 
  Filereader.read_java str (function lexbuf -> Compile.execute lexbuf !verbose)

let () =
  Arg.parse ["-v", Arg.Set verbose, "verbose mode"] compile "Usage: commentJava [-v] <input file>"
