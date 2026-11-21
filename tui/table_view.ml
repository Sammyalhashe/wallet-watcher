(* Renders the position table with per-asset subtotals. *)

open Notty
open Wallet_watcher
open Palette

let headers = [| "ASSET"; "NETWORK"; "WALLET"; "ADDRESS"; "AMOUNT"; "USD VALUE"; "SHARE %" |]
let aligns = [| `Left; `Left; `Left; `Left; `Right; `Right; `Right |]

let position_row (r : Portfolio.row) =
  let p = r.position in
  let usd = match r.usd_value with Some v -> (Format.fmt_usd v, a_usd) | None -> ("-", a_dim) in
  let share = match r.share with Some s -> (Format.fmt_share s, a_share) | None -> ("-", a_dim) in
  let network = Config.string_of_network p.network in
  [| (Format.truncate 12 p.asset, a_dim);
     ( Format.truncate 32
         (match p.detail with Some d -> network ^ " - " ^ d | None -> network),
       a_dim );
     (Format.truncate 18 p.wallet, a_label);
     (Format.short_address p.address, a_dim);
     (Format.fmt_amount p.amount, a_amount);
     usd;
     share |]

let subtotal_row (b : Portfolio.asset_block) =
  [| (Format.truncate 12 b.coin, a_subtotal);
     ("", a_dim);
     ("TOTAL", a_subtotal);
     ( (match (b.price, b.subtotal_usd) with
        | Some p, _ -> "@ " ^ Format.fmt_usd p
        | None, Some _ -> ""  (* valued by the source, not a spot price *)
        | None, None -> "price unavailable"),
       a_dim );
     (Format.fmt_amount b.subtotal_amount, a_subtotal);
     ((match b.subtotal_usd with Some v -> Format.fmt_usd v | None -> "-"), a_subtotal);
     ((if b.subtotal_amount > 0. then "100.00%" else "-"), a_subtotal) |]

let table_rows p =
  List.concat_map
    (fun (b : Portfolio.asset_block) -> List.map position_row b.rows @ [ subtotal_row b ])
    (Portfolio.table p)

let column_widths rows =
  let widths = Array.map String.length headers in
  List.iter
    (fun row ->
       Array.iteri
         (fun i (text, _) ->
            if String.length text > widths.(i) then widths.(i) <- String.length text)
         row)
    rows;
  widths

let render_row widths row =
  Array.to_list row
  |> List.mapi (fun i (text, attr) -> I.string attr (Format.pad aligns.(i) widths.(i) text))
  |> List.concat_map (fun img -> [ img; I.string A.empty "  " ])
  |> I.hcat

let render_header widths =
  Array.map (fun text -> (text, a_header)) headers |> render_row widths

let rule width = I.string a_dim (String.make (max 0 width) '-')

let render_table p =
  let rows = table_rows p in
  let widths = column_widths rows in
  let table_width = Array.fold_left (fun acc w -> acc + w + 2) 0 widths in
  ( table_width,
    I.vcat
      [ render_header widths; rule table_width; I.vcat (List.map (render_row widths) rows) ] )
