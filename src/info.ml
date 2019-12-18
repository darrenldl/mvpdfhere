type t = {
  year : int option;
  month : int option;
  day : int option;
  journal : string option;
  title : string option;
}

let empty =
  { year = None; month = None; day = None; journal = None; title = None }

let map_int_field_to_json (x : int option) : Yojson.Basic.t =
  match x with None -> `Null | Some x -> `Int x

let map_string_field_to_json (x : string option) : Yojson.Basic.t =
  match x with None -> `Null | Some x -> `String x

let map_int_field_from_json (x : Yojson.Basic.t) : int option =
  match x with `Int x -> Some x | _ -> None

let map_string_field_from_json (x : Yojson.Basic.t) : string option =
  match x with `String x -> Some x | _ -> None

let to_json (t : t) : Yojson.Basic.t =
  let l =
    [
      ("year", map_int_field_to_json t.year);
      ("month", map_int_field_to_json t.month);
      ("day", map_int_field_to_json t.day);
      ("journal", map_string_field_to_json t.journal);
      ("title", map_string_field_to_json t.title);
    ]
  in
  `Assoc l

let of_json (x : Yojson.Basic.t) : t =
  match x with
  | `Assoc l ->
    l
    |> List.fold_left
      (fun info (k, v) ->
         match k with
         | "year" -> { info with year = map_int_field_from_json v }
         | "month" -> { info with month = map_int_field_from_json v }
         | "day" -> { info with day = map_int_field_from_json v }
         | "journal" -> { info with journal = map_string_field_from_json v }
         | "title" -> { info with title = map_string_field_from_json v }
         | _ -> info)
      empty
  | _ -> failwith "Unexpected case"

let write ~json_path t = Yojson.Basic.to_file json_path (to_json t)

let load ~json_path : t =
  try Yojson.Basic.from_file json_path |> of_json with _ -> empty
