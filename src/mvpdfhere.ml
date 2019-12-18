let () =
  let argv = Sys.argv in
  let arg_count = Array.length argv in
  if arg_count <= 1 then print_endline "Not enough arguments"
  else if arg_count > 2 then print_endline "Too many arguments"
  else
    let path = argv.(1) in
    try
      if Sys.file_exists path then
        let file =
          if Sys.is_directory path then (
            print_endline "Specified path is a directory, grabbing lastest PDF file";
            let sub_files = Sys.readdir path
                            |> Array.to_list
                            |> List.filter (fun s ->
                                Filename.check_suffix s ".pdf"
                              )
                            |> List.map (fun s ->
                                s, Unix.stat (Filename.concat path s)
                              )
                            |> List.sort (fun (_, stats1) (_, stats2) ->
                                let open Unix in
                                compare stats2.st_mtime stats1.st_mtime
                              )
                            |> List.map (fun (s, _) -> s)
            in
            List.hd sub_files
          )
          else path
        in
        let text_dump_path = Filename.temp_file "mvpdfhere" ".txt" in
        let json_path = Filename.temp_file "mvpdfhere" ".json" in
        print_endline file
      else print_endline (Printf.sprintf "File \"%s\" does not exist" path)
    with Sys_error _ -> print_endline "Failed to access file"
