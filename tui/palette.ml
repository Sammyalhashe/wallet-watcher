(* Text attributes, series colours and glyphs. *)

open Notty

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
