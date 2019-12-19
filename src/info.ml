type t = {
  year : int option;
  month : int option;
  day : int option;
  journal_or_conference : string option;
  publisher : string option;
  authors : string list;
  title : string option;
}

let empty =
  {
    year = None;
    month = None;
    day = None;
    journal_or_conference = None;
    publisher = None;
    authors = [];
    title = None;
  }

let map_int_field_to_json (x : int option) : Yojson.Basic.t =
  match x with None -> `Null | Some x -> `Int x

let map_string_field_to_json (x : string option) : Yojson.Basic.t =
  match x with None -> `String "" | Some x -> `String x

let map_int_field_to_json_string (x : int option) : string =
  match x with None -> "null" | Some x -> string_of_int x

let map_string_field_to_json_string (x : string option) : string =
  match x with None -> "\"\"" | Some x -> Printf.sprintf "\"%s\"" x

let map_string_list_field_to_json_string (l : string list) : string =
  "[" ^ String.concat "," l ^ "]"

let map_int_field_from_json (x : Yojson.Basic.t) : int option =
  match x with `Int x -> Some x | _ -> None

let map_string_field_from_json (x : Yojson.Basic.t) : string option =
  match x with `String "" -> None | `String x -> Some x | _ -> None

let map_string_list_field_from_json (x : Yojson.Basic.t) : string list =
  match x with
  | `List l ->
    l
    |> List.filter_map (fun x ->
        match x with `String s -> Some s | _ -> None)
  | _ -> []

(* let to_json (t : t) : Yojson.Basic.t =
 *   let l =
 *     [
 *       ("year", map_int_field_to_json t.year);
 *       ("month", map_int_field_to_json t.month);
 *       ("day", map_int_field_to_json t.day);
 *       ("journal_or_conference", map_string_field_to_json t.journal_or_conference);
 *       ("title", map_string_field_to_json t.title);
 *     ]
 *   in
 *   `Assoc l *)

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
         | "journal_or_conference" ->
           {
             info with
             journal_or_conference = map_string_field_from_json v;
           }
         | "publisher" ->
           { info with publisher = map_string_field_from_json v }
         | "authors" ->
           { info with authors = map_string_list_field_from_json v }
         | "title" -> { info with title = map_string_field_from_json v }
         | _ -> info)
      empty
  | _ -> empty

let write ~json_path t =
  let oc = open_out json_path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () ->
       let fields_str =
         [
           ("year", map_int_field_to_json_string t.year);
           ("month", map_int_field_to_json_string t.month);
           ("day", map_int_field_to_json_string t.day);
           ( "journal_or_conference",
             map_string_field_to_json_string t.journal_or_conference );
           ("publisher", map_string_field_to_json_string t.publisher);
           ("authors", map_string_list_field_to_json_string t.authors);
           ("title", map_string_field_to_json_string t.title);
         ]
         |> List.map (fun (k, v) -> Printf.sprintf "  \"%s\": %s" k v)
         |> String.concat ",\n"
       in

       Printf.fprintf oc "{\n";
       Printf.fprintf oc "%s\n" fields_str;
       Printf.fprintf oc "}\n")

let load ~json_path : t =
  try Yojson.Basic.from_file json_path |> of_json with _ -> empty
