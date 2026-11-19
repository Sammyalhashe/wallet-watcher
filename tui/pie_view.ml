(* Renders the pie chart, its legend and the drill-down breadcrumb. *)

open Notty
open Wallet_watcher
open Palette

let breadcrumb = function
  | Nav.Root_by_coin -> "NET WORTH > by coin"
  | Nav.Root_by_wallet -> "NET WORTH > by wallet"
  | Nav.Coin asset -> Printf.sprintf "NET WORTH > %s by wallet" (Config.string_of_asset asset)
  | Nav.Wallet label -> Printf.sprintf "NET WORTH > %s by coin" label

let pie_rows radius = ((2 * radius) + 3) / 4
let pie_cols radius = ((2 * radius) + 1) / 2

let pie_radius (width, height) ~chrome =
  let rows_available = height - chrome in
  max 8 (min 16 (min (2 * rows_available) (width - 50)))

(* An empty or short grid (no Pie implementation yet) renders as blanks. *)
let pie_image radius slices =
  let weights =
    List.map (fun (s : Portfolio.slice) -> Option.value s.pct ~default:0.) slices
  in
  let grid = Pie.cells ~radius weights in
  let cell row col =
    let lit =
      if row < Array.length grid && col < Array.length grid.(row) then grid.(row).(col)
      else None
    in
    match lit with
    | Some (c : Pie.cell) ->
      I.uchar series_attrs.(c.slice mod Array.length series_attrs) c.glyph 1 1
    | None -> I.void 1 1
  in
  I.vcat
    (List.init (pie_rows radius) (fun row ->
         I.hcat (List.init (pie_cols radius) (cell row))))

let legend_row ~selected ~widths:(name_w, pct_w, amount_w, usd_w) i (s : Portfolio.slice) =
  let styled attr text =
    I.string (if selected then A.(attr ++ st reverse) else attr) text
  in
  let pct = match s.pct with Some p -> Format.fmt_share p | None -> "-" in
  let amount = match s.amount with Some a -> Format.fmt_amount a | None -> "" in
  let usd = match s.usd with Some v -> Format.fmt_usd v | None -> "-" in
  I.hcat
    [ styled A.empty " ";
      styled series_attrs.(i mod Array.length series_attrs)
        (String.make 1 series_glyphs.(i mod max_slices));
      styled A.empty " ";
      styled a_label (Format.pad `Left name_w s.name);
      styled A.empty "  ";
      styled (if s.pct = None then a_dim else a_share) (Format.pad `Right pct_w pct);
      styled A.empty "  ";
      styled a_amount (Format.pad `Right amount_w amount);
      styled A.empty "  ";
      styled (if s.usd = None then a_dim else a_usd) (Format.pad `Right usd_w usd);
      styled A.empty " " ]

let legend_image ~selected slices =
  let widest f = List.fold_left (fun acc s -> max acc (String.length (f s))) 0 slices in
  let widths =
    ( min 20 (widest (fun (s : Portfolio.slice) -> s.name)),
      widest (fun (s : Portfolio.slice) ->
          match s.pct with Some p -> Format.fmt_share p | None -> "-"),
      widest (fun (s : Portfolio.slice) ->
          match s.amount with Some a -> Format.fmt_amount a | None -> ""),
      widest (fun (s : Portfolio.slice) ->
          match s.usd with Some v -> Format.fmt_usd v | None -> "-") )
  in
  let truncated =
    List.map (fun (s : Portfolio.slice) -> { s with name = Format.truncate 20 s.name }) slices
  in
  I.vcat (List.mapi (fun i s -> legend_row ~selected:(i = selected) ~widths i s) truncated)

let render_pie state p size ~chrome =
  let slices = Nav.rows p state.Nav.focus in
  let crumb = I.string a_header (breadcrumb state.Nav.focus) in
  if slices = [] then I.vcat [ crumb; I.void 1 1; I.string a_dim "  nothing to show" ]
  else begin
    let radius = pie_radius size ~chrome in
    I.vcat
      [ crumb;
        I.void 1 1;
        I.hcat [ pie_image radius slices; I.void 3 1; legend_image ~selected:state.Nav.selected slices ] ]
  end
