(* Total JSON and hex helpers: malformed responses surface as [Error], never raise. *)

let ( let* ) = Result.bind

(* Float, not int: 1 ETH is 10^18 wei and native int overflows near 4.6e18. *)
let float_of_hex s =
  let s =
    if String.length s >= 2 && s.[0] = '0' && (s.[1] = 'x' || s.[1] = 'X') then
      String.sub s 2 (String.length s - 2)
    else s
  in
  if s = "" then Error "empty hex quantity"
  else begin
    let acc = ref 0. and bad = ref None in
    String.iter
      (fun c ->
         let d =
           match c with
           | '0' .. '9' -> Some (Char.code c - Char.code '0')
           | 'a' .. 'f' -> Some (Char.code c - Char.code 'a' + 10)
           | 'A' .. 'F' -> Some (Char.code c - Char.code 'A' + 10)
           | _ -> None
         in
         match d with
         | Some d -> acc := (!acc *. 16.) +. float_of_int d
         | None -> if !bad = None then bad := Some c)
      s;
    match !bad with
    | Some c -> Error (Printf.sprintf "invalid hex digit %C" c)
    | None -> Ok !acc
  end

let member key = function
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

let rec path keys json =
  match keys with
  | [] -> Some json
  | k :: rest -> ( match member k json with Some v -> path rest v | None -> None)

let as_number = function
  | `Int i -> Some (float_of_int i)
  | `Float f -> Some f
  | `String s -> float_of_string_opt s
  | _ -> None

let missing keys = Error (Printf.sprintf "missing field %s" (String.concat "." keys))

let number_at keys json =
  match path keys json with
  | None -> missing keys
  | Some v -> (
    match as_number v with
    | Some f -> Ok f
    | None -> Error (Printf.sprintf "field %s is not a number" (String.concat "." keys)))

let single_line s =
  String.map (fun c -> if Char.code c < 0x20 || Char.code c = 0x7f then ' ' else c) s

let parse_json body =
  match Yojson.Basic.from_string body with
  | json -> Ok json
  | exception Yojson.Json_error msg -> Error ("invalid JSON: " ^ single_line msg)
