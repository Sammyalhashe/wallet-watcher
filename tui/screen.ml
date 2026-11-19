(* Composes banner, body and footer into the full screen image. *)

open Notty
open Wallet_watcher
open Palette

let banner (p : Portfolio.t) =
  I.hcat [ I.string a_header "NET WORTH  "; I.string a_banner (Format.fmt_usd p.total_usd) ]

let hint = function
  | { Nav.view = Table; _ } -> "c pie view   q quit"
  | { Nav.focus = Root_by_coin | Root_by_wallet; _ } ->
    "j/k move   Enter drill   Tab coin/wallet   c table   q quit"
  | _ -> "j/k move   Backspace up   c table   q quit"

let footer state inner (p : Portfolio.t) =
  let notes =
    (match p.price_error with
     | Some e -> [ I.string a_error ("prices: " ^ Format.truncate (inner - 2) e) ]
     | None -> [])
    @ List.map
        (fun (f : Portfolio.failure) ->
           I.string a_error
             (Format.truncate inner
                (Printf.sprintf "%s %s: %s" (Config.string_of_network f.network)
                   f.wallet f.error)))
        p.failures
  in
  I.vcat (notes @ [ I.string a_dim (hint state) ])

let render state (p : Portfolio.t) ((width, _) as size) =
  let title = I.string a_title "  WALLET WATCHER  " in
  let footer_lines =
    List.length p.failures + (if p.price_error = None then 0 else 1) + 1
  in
  let chrome = 8 + footer_lines in
  let body_width, body =
    match state.Nav.view with
    | Table -> Table_view.render_table p
    | Pie -> (min (max width 40) 100, Pie_view.render_pie state p size ~chrome)
  in
  let inner = max body_width (min width 100) in
  I.vcat [ title; banner p; Table_view.rule inner; body; Table_view.rule inner; footer state inner p ]
  |> I.pad ~l:1 ~t:1
