let capitalize s =
  s |> String.split_on_char '-'
  |> List.map String.capitalize_ascii
  |> String.concat "-"

let replace_with_predefined_rules s =
  let re_s =
    List.map
      (fun (map_from, map_to) -> (Str.regexp_string map_from, map_to))
      Config.normalize_replacements
  in
  List.fold_left
    (fun s (map_from, map_to) -> Str.global_replace map_from map_to s)
    s re_s

let normalize (s : string) : string =
  s |> String.split_on_char ' '
  |> List.filter (fun s -> s <> "")
  |> List.map replace_with_predefined_rules
  |> List.map (fun s ->
      if s = String.uppercase_ascii s then s else String.lowercase_ascii s)
  |> List.map String.to_seq
  |> List.map
    (Seq.filter (fun c -> not (String.contains Config.excluded_chars c)))
  |> List.map String.of_seq
  |> List.map (fun s ->
      if List.mem s Config.words_stay_lowercase then s else capitalize s)
  |> String.concat "_"

let split_name_into_first_last (s : string) : (string option * string) option =
  if String.contains s ',' then
    let parts =
      s |> String.split_on_char ',' |> List.filter (fun s -> s <> "")
    in
    match parts with
    | [] -> failwith "Unexpected case"
    | [ last ] -> Some (None, last)
    | l -> Some (Some (l |> List.rev |> List.hd), List.hd l)
  else
    let parts =
      s |> String.split_on_char ' ' |> List.filter (fun s -> s <> "")
    in
    match parts with
    | [] -> None
    | [ last ] -> Some (None, last)
    | l -> Some (Some (List.hd l), l |> List.rev |> List.hd)

let normalize_names l =
  List.filter_map
    (fun s ->
       split_name_into_first_last s
       |> Option.map (fun (first, last) ->
           let first = Option.map normalize first in
           let last = normalize last in
           match first with
           | None -> last
           | Some first -> Printf.sprintf "%s_%s" first last))
    l
