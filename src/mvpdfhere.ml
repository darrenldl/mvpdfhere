let () =
  let argv = Sys.argv in
  let arg_count = Array.length argv in
  if arg_count <= 1 then print_endline "Not enough arguments"
  else if arg_count > 2 then print_endline "Too many arguments"
  else
    let file = argv.(1) in
    try
      if Sys.file_exists file then
        let file =
          if Sys.is_directory file then (
            print_endline "Specified file is a directory, grabbing lastest file";
            "" )
          else file
        in
        ()
      else print_endline (Printf.sprintf "File \"%s\" does not exist" file)
    with Sys_error _ -> print_endline "Failed to access file"
