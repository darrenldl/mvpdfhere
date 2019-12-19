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
  |> List.map (fun s ->
      if s = String.uppercase_ascii s then s else String.lowercase_ascii s)
  |> List.map String.to_seq
  |> List.map
    (Seq.filter (fun c -> not (String.contains Config.excluded_chars c)))
  |> List.map String.of_seq
  |> List.map (fun s ->
      if List.mem s Config.words_stay_lowercase then s else capitalize s)
  |> String.concat "_"
