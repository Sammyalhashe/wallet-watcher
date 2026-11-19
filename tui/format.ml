(* String and number formatting for table and legend text. *)

(* Padding is computed with [String.length], so every text cell must stay
   ASCII: a multi-byte glyph would count as several columns here but render as
   one, skewing the layout. Only the pie cells are non-ASCII. *)

let truncate width s =
  if String.length s <= width then s else String.sub s 0 (max 0 (width - 2)) ^ ".."

let pad align width s =
  let n = String.length s in
  if n >= width then s
  else
    let fill = String.make (width - n) ' ' in
    match align with `Left -> s ^ fill | `Right -> fill ^ s

let short_address a =
  let n = String.length a in
  if n <= 20 then a else String.sub a 0 10 ^ ".." ^ String.sub a (n - 6) 6

let commas digits =
  let n = String.length digits in
  let buf = Buffer.create (n + (n / 3)) in
  String.iteri
    (fun i c ->
       if i > 0 && (n - i) mod 3 = 0 then Buffer.add_char buf ',';
       Buffer.add_char buf c)
    digits;
  Buffer.contents buf

let fmt_usd v =
  let s = Printf.sprintf "%.2f" v in
  match String.index_opt s '.' with
  | Some i ->
    "$" ^ commas (String.sub s 0 i) ^ String.sub s i (String.length s - i)
  | None -> "$" ^ commas s

let fmt_amount v = Printf.sprintf "%.6f" v
let fmt_share p = Printf.sprintf "%.2f%%" p
