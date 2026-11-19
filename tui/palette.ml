(* Text attributes, series colours and glyphs, and the slice-folding rule. *)

open Notty
open Wallet_watcher

let a_title = A.(fg lightgreen ++ st bold)
let a_header = A.(fg white ++ st bold)
let a_label = A.fg A.white
let a_dim = A.fg A.lightblack
let a_amount = A.fg A.cyan
let a_usd = A.fg A.yellow
let a_share = A.(fg green ++ st bold)
let a_subtotal = A.(fg white ++ st bold)
let a_error = A.fg A.red
let a_banner = A.(fg lightgreen ++ st bold)

(* A slice's colour follows its rank and is never cycled: red is reserved for
   errors, and each series also has an ASCII glyph so the legend reads without
   colour. *)
let series_attrs =
  [| A.fg A.cyan; A.fg A.yellow; A.fg A.magenta; A.fg A.blue;
     A.fg A.lightcyan; A.fg A.lightyellow; A.fg A.lightmagenta; A.fg A.lightblue |]

let series_glyphs = [| '#'; '='; '+'; '*'; 'o'; 'x'; ':'; '~' |]
let max_slices = Array.length series_glyphs

(* Beyond the palette the tail folds into one bucket instead of reusing a colour. *)
let fold_extra (slices : Portfolio.slice list) : Portfolio.slice list =
  if List.length slices <= max_slices then slices
  else begin
    let keep = List.filteri (fun i _ -> i < max_slices - 1) slices in
    let rest = List.filteri (fun i _ -> i >= max_slices - 1) slices in
    let sum_known project =
      List.fold_left
        (fun acc s ->
           match (acc, project s) with
           | None, v -> v
           | Some a, Some b -> Some (a +. b)
           | acc, None -> acc)
        None rest
    in
    keep
    @ [ { Portfolio.name = Printf.sprintf "Other (%d)" (List.length rest);
          amount = None;
          usd = sum_known (fun (s : Portfolio.slice) -> s.usd);
          pct = sum_known (fun (s : Portfolio.slice) -> s.pct) } ]
  end
