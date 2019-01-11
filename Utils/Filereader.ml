let get_file str =
  let temp2 = Filename.check_suffix str ".java" in
  let file = (if temp2 then str else str^".java") in
  let filename = 
    begin
      try
	let idx = String.rindex str '/' in
	let temp1 = String.sub str (idx + 1) ((String.length str) - idx - 1) in
	if temp2 then Filename.chop_suffix temp1 ".java" else temp1
      with Not_found ->
	if temp2 then Filename.chop_suffix str ".java" else str
    end
  in
  file, filename

let read_java str reader =
  let (file, filename) = get_file str in
  try 
    let input_file = print_endline ("Open"); open_in file in
    try
      let lexbuf = Lexing.from_channel input_file in
      Location.init lexbuf file;
      reader lexbuf;
      close_in (input_file);
      print_endline ("Close")
    with s ->
      close_in (input_file);
      print_endline ("Close");
      raise s
  with Sys_error s ->
    print_endline ("Can't find file '" ^ file ^ "'")