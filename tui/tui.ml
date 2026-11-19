(* Notty rendering and the Lwt event loop. *)

open Notty
open Lwt.Infix
open Wallet_watcher

(* {1 Formatting}

   Padding is computed with [String.length], so every text cell must stay
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

(* {1 Palette} *)

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

(* {1 State and transitions} *)

type view = Table | Pie

type focus =
  | Root_by_coin
  | Root_by_wallet
  | Coin of Config.asset
  | Wallet of string

type state = { view : view; focus : focus; selected : int }

type event =
  | Toggle_view
  | Switch_root
  | Move of int
  | Drill
  | Up
  | Quit

let initial = { view = Table; focus = Root_by_coin; selected = 0 }

let slices_of (p : Portfolio.t) = function
  | Root_by_coin -> Portfolio.by_asset p
  | Root_by_wallet -> Portfolio.by_wallet p
  | Coin asset -> Portfolio.asset_by_wallet p asset
  | Wallet label -> Portfolio.wallet_by_asset p label

let rows p focus = fold_extra (slices_of p focus)

let index_of_name p focus name =
  let rec find i = function
    | [] -> 0
    | (s : Portfolio.slice) :: _ when s.name = name -> i
    | _ :: rest -> find (i + 1) rest
  in
  find 0 (rows p focus)

let selected_name p state =
  List.nth_opt (rows p state.focus) state.selected
  |> Option.map (fun (s : Portfolio.slice) -> s.name)

let drill_target p state =
  match (state.focus, selected_name p state) with
  | Root_by_coin, Some name ->
    List.find_opt (fun a -> Config.string_of_asset a = name) (Portfolio.assets p)
    |> Option.map (fun a -> Coin a)
  | Root_by_wallet, Some name ->
    List.find_opt (String.equal name) (Portfolio.wallets p)
    |> Option.map (fun l -> Wallet l)
  | _ -> None

let step p state = function
  | Quit -> None
  | Toggle_view ->
    Some { state with view = (match state.view with Table -> Pie | Pie -> Table) }
  | _ when state.view = Table -> Some state
  | Switch_root -> (
    match state.focus with
    | Root_by_coin -> Some { state with focus = Root_by_wallet; selected = 0 }
    | Root_by_wallet -> Some { state with focus = Root_by_coin; selected = 0 }
    | Coin _ | Wallet _ -> Some state)
  | Move delta ->
    let last = max 0 (List.length (rows p state.focus) - 1) in
    Some { state with selected = max 0 (min last (state.selected + delta)) }
  | Drill -> (
    match drill_target p state with
    | Some focus -> Some { state with focus; selected = 0 }
    | None -> Some state)
  | Up -> (
    match state.focus with
    | Root_by_coin | Root_by_wallet -> Some state
    | Coin asset ->
      Some { state with focus = Root_by_coin;
                        selected = index_of_name p Root_by_coin (Config.string_of_asset asset) }
    | Wallet label ->
      Some { state with focus = Root_by_wallet;
                        selected = index_of_name p Root_by_wallet label })

let event_of_input = function
  | `Key (`ASCII ('c' | 'C'), mods) ->
    if List.mem `Ctrl mods then Some Quit else Some Toggle_view
  | `Key (`ASCII 'q', _) -> Some Quit
  | `Key (`Tab, _) -> Some Switch_root
  | `Key ((`ASCII 'j' | `Arrow `Down), _) -> Some (Move 1)
  | `Key ((`ASCII 'k' | `Arrow `Up), _) -> Some (Move (-1))
  | `Key ((`Enter | `ASCII 'l'), _) -> Some Drill
  | `Key ((`Backspace | `ASCII 'h' | `Escape), _) -> Some Up
  | _ -> None

(* {1 Table} *)

let headers = [| "ASSET"; "NETWORK"; "WALLET"; "ADDRESS"; "AMOUNT"; "USD VALUE"; "SHARE %" |]
let aligns = [| `Left; `Left; `Left; `Left; `Right; `Right; `Right |]

let position_row (r : Portfolio.row) =
  let p = r.position in
  let usd = match r.usd_value with Some v -> (fmt_usd v, a_usd) | None -> ("-", a_dim) in
  let share = match r.share with Some s -> (fmt_share s, a_share) | None -> ("-", a_dim) in
  [| (Config.string_of_asset p.asset, a_dim);
     (Config.string_of_network p.network, a_dim);
     (truncate 18 p.wallet, a_label);
     (short_address p.address, a_dim);
     (fmt_amount p.amount, a_amount);
     usd;
     share |]

let subtotal_row (b : Portfolio.asset_block) =
  [| (Config.string_of_asset b.coin, a_subtotal);
     ("", a_dim);
     ("TOTAL", a_subtotal);
     ( (match b.price with
        | Some p -> "@ " ^ fmt_usd p
        | None -> "price unavailable"),
       a_dim );
     (fmt_amount b.subtotal_amount, a_subtotal);
     ((match b.subtotal_usd with Some v -> fmt_usd v | None -> "-"), a_subtotal);
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
  |> List.mapi (fun i (text, attr) -> I.string attr (pad aligns.(i) widths.(i) text))
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

(* {1 Pie} *)

let breadcrumb = function
  | Root_by_coin -> "NET WORTH > by coin"
  | Root_by_wallet -> "NET WORTH > by wallet"
  | Coin asset -> Printf.sprintf "NET WORTH > %s by wallet" (Config.string_of_asset asset)
  | Wallet label -> Printf.sprintf "NET WORTH > %s by coin" label

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
  let pct = match s.pct with Some p -> fmt_share p | None -> "-" in
  let amount = match s.amount with Some a -> fmt_amount a | None -> "" in
  let usd = match s.usd with Some v -> fmt_usd v | None -> "-" in
  I.hcat
    [ styled A.empty " ";
      styled series_attrs.(i mod Array.length series_attrs)
        (String.make 1 series_glyphs.(i mod max_slices));
      styled A.empty " ";
      styled a_label (pad `Left name_w s.name);
      styled A.empty "  ";
      styled (if s.pct = None then a_dim else a_share) (pad `Right pct_w pct);
      styled A.empty "  ";
      styled a_amount (pad `Right amount_w amount);
      styled A.empty "  ";
      styled (if s.usd = None then a_dim else a_usd) (pad `Right usd_w usd);
      styled A.empty " " ]

let legend_image ~selected slices =
  let widest f = List.fold_left (fun acc s -> max acc (String.length (f s))) 0 slices in
  let widths =
    ( min 20 (widest (fun (s : Portfolio.slice) -> s.name)),
      widest (fun (s : Portfolio.slice) ->
          match s.pct with Some p -> fmt_share p | None -> "-"),
      widest (fun (s : Portfolio.slice) ->
          match s.amount with Some a -> fmt_amount a | None -> ""),
      widest (fun (s : Portfolio.slice) ->
          match s.usd with Some v -> fmt_usd v | None -> "-") )
  in
  let truncated =
    List.map (fun (s : Portfolio.slice) -> { s with name = truncate 20 s.name }) slices
  in
  I.vcat (List.mapi (fun i s -> legend_row ~selected:(i = selected) ~widths i s) truncated)

let render_pie state p size ~chrome =
  let slices = rows p state.focus in
  let crumb = I.string a_header (breadcrumb state.focus) in
  if slices = [] then I.vcat [ crumb; I.void 1 1; I.string a_dim "  nothing to show" ]
  else begin
    let radius = pie_radius size ~chrome in
    I.vcat
      [ crumb;
        I.void 1 1;
        I.hcat [ pie_image radius slices; I.void 3 1; legend_image ~selected:state.selected slices ] ]
  end

(* {1 Screen} *)

let banner (p : Portfolio.t) =
  I.hcat [ I.string a_header "NET WORTH  "; I.string a_banner (fmt_usd p.total_usd) ]

let hint = function
  | { view = Table; _ } -> "c pie view   q quit"
  | { focus = Root_by_coin | Root_by_wallet; _ } ->
    "j/k move   Enter drill   Tab coin/wallet   c table   q quit"
  | _ -> "j/k move   Backspace up   c table   q quit"

let footer state inner (p : Portfolio.t) =
  let notes =
    (match p.price_error with
     | Some e -> [ I.string a_error ("prices: " ^ truncate (inner - 2) e) ]
     | None -> [])
    @ List.map
        (fun (f : Portfolio.failure) ->
           I.string a_error
             (truncate inner
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
    match state.view with
    | Table -> render_table p
    | Pie -> (min (max width 40) 100, render_pie state p size ~chrome)
  in
  let inner = max body_width (min width 100) in
  I.vcat [ title; banner p; rule inner; body; rule inner; footer state inner p ]
  |> I.pad ~l:1 ~t:1

(* {1 Event loop} *)

let run (p : Portfolio.t) =
  let term = Notty_lwt.Term.create () in
  let state = ref initial in
  let draw () = Notty_lwt.Term.image term (render !state p (Notty_lwt.Term.size term)) in
  let events = Notty_lwt.Term.events term in
  let rec loop () =
    Lwt_stream.get events >>= function
    | None -> Lwt.return_unit
    (* Notty does not redraw on resize; re-render against the new size. *)
    | Some (`Resize _) -> draw () >>= loop
    | Some input -> (
      match event_of_input input with
      | None -> loop ()
      | Some event -> (
        match step p !state event with
        | None -> Lwt.return_unit
        | Some next -> state := next; draw () >>= loop))
  in
  draw () >>= loop >>= fun () -> Notty_lwt.Term.release term
