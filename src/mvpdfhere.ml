let () =
  let argv = Sys.argv in
  let arg_count = Array.length argv in
  if arg_count = 0 then
    print_endline "No arguments received"
  else if arg_count > 1 then
    print_endline "Too many arguments"
  else
    let file = argv.(0) in
    ()
