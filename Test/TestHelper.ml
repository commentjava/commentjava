let successCount = ref 0
let failCount = ref 0

let test_file assert_fct file =
  (* Run assert_fct on the file, and increment the fail counter if there is an exception, else increment the success counter *)
  try
    print_endline ("> Testing " ^ file);
    assert_fct file;
    successCount := !successCount + 1; print_endline ("\n> " ^ file ^ " passed \n")
  with
    _ -> failCount := !failCount + 1; print_endline ("\n/!\\/!\\ " ^ file ^ " failed /!\\/!\\\n")

let dir_is_empty dir =
  (* Return true if dir is empty except . and .. *)
  Array.length (Sys.readdir dir) = 0

let dir_contents dir =
  (* Return a list of files in dir and it subdirectories *)
  let rec loop result = function
    | f::fs when Sys.is_directory f ->
          Sys.readdir f
          |> Array.to_list
          |> List.map (Filename.concat f)
          |> List.append fs
          |> loop result
    | f::fs -> loop (f::result) fs
    | []    -> result
  in
    loop [] [dir]

let rec filter_java files = 
  match files with
    | [] -> []
    | hd :: tl when Filename.check_suffix hd ".java" -> hd :: (filter_java tl)
    | hd :: tl -> filter_java tl

let test_dir dir assert_fct =
  (* Run the assert_fct for each file in the *)
  if dir_is_empty dir then
    print_endline ("There is no file to test in " ^ dir)
  else
    let files = filter_java (dir_contents dir)
    in
      List.iter (test_file assert_fct) files;
      print_endline (">>> Total tests: " ^ (string_of_int (!successCount + !failCount)));
      print_endline (">>> Passed: " ^ (string_of_int !successCount));
      print_endline (">>> Failed: " ^ (string_of_int !failCount))
