type yns =
  [ `Yes
  | `No
  | `Stop
  ]

type action =
  [ `Stop of string option
  | `Retry
  ]

let run_command cmd = match Sys.command cmd with 0 -> Ok () | x -> Error x

let write_text_dump ~pdf_file_path ~text_dump_path : (unit, int) result =
  run_command
    (Printf.sprintf "pdftotext -f 1 -l 1 -- \"%s\" - | head -n 20 > %s"
       pdf_file_path text_dump_path)

let ask_yns ~prompt : yns =
  let rec aux prompt answer =
    match answer with
    | Some x -> x
    | None -> (
        Printf.printf "%s y/n/s : " prompt;
        let res = try read_line () with End_of_file -> "" in
        match res with
        | "y" -> `Yes
        | "n" -> `No
        | "s" -> `Stop
        | _ ->
          print_endline "Invalid answer";
          aux prompt None )
  in
  aux prompt None

let edit_loop ~pdf_file_path : (string, unit) result =
  let rec aux ~pdf_file_path =
    let text_dump_path = Filename.temp_file "mvpdfhere" ".txt" in
    let raw_json_path = Filename.temp_file "mvpdfhere" ".json" in
    let json_path = Filename.temp_file "mvpdfhere" ".json" in
    let name =
      Fun.protect
        ~finally:(fun () ->
            Sys.remove text_dump_path;
            Sys.remove json_path)
        (fun () ->
           match write_text_dump ~pdf_file_path ~text_dump_path with
           | Error x ->
             Printf.printf "Text dump command exited with return code %d\n" x;
             Error ()
           | Ok () -> (
               Info.write ~json_path:raw_json_path Info.empty;
               match
                 run_command
                   (Printf.sprintf "cat %s | jq > %s" raw_json_path json_path)
               with
               | Error x ->
                 Printf.printf "Jq exited with return code %d" x;
                 Error ()
               | Ok () -> (
                   match
                     run_command
                       (Printf.sprintf "vim -O %s %s" json_path text_dump_path)
                   with
                   | Error x ->
                     Printf.printf "Vim exited with return code %d\n" x;
                     Error ()
                   | Ok () -> (
                       let info = Info.load ~json_path in
                       let parts =
                         [
                           Option.map string_of_int info.year;
                           Option.map string_of_int info.month;
                           Option.map string_of_int info.day;
                           info.journal;
                           info.title;
                           Some ".pdf";
                         ]
                         |> List.filter_map (fun x -> x)
                       in
                       let name = String.concat "__" parts in
                       Printf.printf "Computed file name is : \"%s\"\n" name;
                       match
                         ask_yns
                           ~prompt:
                             (Printf.sprintf
                                "Move PDF file to current dir using above name \
                                 (n = redo, s = stop)?")
                       with
                       | `Yes -> Ok (`Stop (Some name))
                       | `Stop -> Ok (`Stop None)
                       | `No -> Ok `Retry ) ) ))
    in
    match name with
    | Error () -> Error ()
    | Ok (`Stop None) -> Error ()
    | Ok (`Stop (Some x)) -> Ok x
    | Ok `Retry -> aux ~pdf_file_path
  in
  aux ~pdf_file_path

let () =
  let argv = Sys.argv in
  let arg_count = Array.length argv in
  if arg_count <= 1 then print_endline "Not enough arguments"
  else if arg_count > 2 then print_endline "Too many arguments"
  else
    let path = argv.(1) in
    try
      if Sys.file_exists path then
        let pdf_file_path =
          if Sys.is_directory path then (
            print_endline
              "Specified path is a directory, grabbing the lastest PDF file";
            let sub_files =
              Sys.readdir path |> Array.to_list
              |> List.filter_map (fun s ->
                  if Filename.check_suffix s ".pdf" then
                    Some (Filename.concat path s)
                  else None)
              |> List.map (fun s -> (s, Unix.stat s))
              |> List.sort (fun (_, stats1) (_, stats2) ->
                  Unix.(compare stats2.st_mtime stats1.st_mtime))
              |> List.map (fun (s, _) -> s)
            in
            List.hd sub_files )
          else path
        in
        match edit_loop ~pdf_file_path with
        | Ok new_file_name -> Sys.rename pdf_file_path new_file_name
        | Error () -> ()
      else print_endline (Printf.sprintf "File \"%s\" does not exist" path)
    with Sys_error _ -> print_endline "Failed to access file"
