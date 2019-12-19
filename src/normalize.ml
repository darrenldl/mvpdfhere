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
      if List.mem s Config.words_stay_lowercase then s
      else String.capitalize_ascii s)
  |> String.concat "_"
