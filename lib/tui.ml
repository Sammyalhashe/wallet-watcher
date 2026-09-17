(* Notty rendering and the Lwt event loop. *)

open Notty
open Lwt.Infix

(* {1 Formatting}

   Padding is computed with [String.length], so every cell must stay ASCII:
   a multi-byte glyph would count as several columns here but render as one,
   skewing the table. *)

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
let a_share = A.(fg green ++ st bold) (* spec: shares in bold green *)
let a_subtotal = A.(fg white ++ st bold)
let a_error = A.fg A.red
let a_banner = A.(fg lightgreen ++ st bold)

(* Categorical series colours, assigned in fixed order and never cycled: a
   slice's colour follows the entity, not its rank, so re-sorting or a dropped
   asset never repaints the others. Red is reserved for errors. Each series also
   carries a distinct ASCII glyph, so the bars stay readable without colour. *)
let series_attrs =
  [| A.fg A.cyan; A.fg A.yellow; A.fg A.magenta; A.fg A.blue;
     A.fg A.lightcyan; A.fg A.lightyellow; A.fg A.lightmagenta; A.fg A.lightblue |]

let series_glyphs = [| '#'; '='; '+'; '*'; 'o'; 'x'; ':'; '~' |]
let max_slices = Array.length series_glyphs

(* {1 Table}

   A row is one cell per column; widths are derived from the widest cell so the
   table is self-sizing. *)

let headers = [| "ASSET"; "WALLET"; "ADDRESS"; "AMOUNT"; "USD VALUE"; "SHARE %" |]
let aligns = [| `Left; `Left; `Left; `Right; `Right; `Right |]

let holding_row chain (h : Portfolio.holding) =
  let amount, share =
    match h.amount with
    | Ok a ->
      ( (fmt_amount a, a_amount),
        match h.share with
        | Some s -> (fmt_share s, a_share)
        | None -> ("-", a_dim) )
    | Error _ -> (("n/a", a_error), ("-", a_dim))
  in
  let usd =
    match h.usd with Some v -> (fmt_usd v, a_usd) | None -> ("-", a_dim)
  in
  [| (chain, a_dim);
     (truncate 18 h.wallet.label, a_label);
     (short_address h.wallet.address, a_dim);
     amount;
     usd;
     share |]

let subtotal_row (a : Portfolio.asset) =
  let usd =
    match a.total_usd with Some v -> fmt_usd v | None -> "-"
  in
  [| (Config.string_of_chain a.chain, a_subtotal);
     ("TOTAL", a_subtotal);
     ( (match a.price with
        | Some p -> "@ " ^ fmt_usd p
        | None -> "price unavailable"),
       a_dim );
     (fmt_amount a.total_amount, a_subtotal);
     (usd, a_subtotal);
     (* Only claim 100% when something was actually counted. *)
     ((if a.total_amount > 0. then "100.00%" else "-"), a_subtotal) |]

let rows_of_portfolio (p : Portfolio.t) =
  List.concat_map
    (fun (a : Portfolio.asset) ->
       let chain = Config.string_of_chain a.chain in
       List.map (holding_row chain) a.holdings @ [ subtotal_row a ])
    p.assets

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
  |> List.mapi (fun i (text, attr) ->
       I.string attr (pad aligns.(i) widths.(i) text))
  |> List.concat_map (fun img -> [ img; I.string A.empty "  " ])
  |> I.hcat

let render_header widths =
  Array.to_list headers
  |> List.map (fun text -> (text, a_header))
  |> Array.of_list
  |> render_row widths

(* {1 Stacked bars}

   Part-to-whole for a handful of categories: one bar plus a legend. A slice
   with an unknown percentage (an asset with no price) is listed in the legend
   but contributes no width. *)

type slice = {
  s_name : string;
  s_pct : float option;
  s_value : string;
}

(* The palette is finite by design; beyond it, the tail folds into one bucket
   rather than reusing a colour that already means something else. *)
let fold_extra slices =
  if List.length slices <= max_slices then slices
  else begin
    let keep = List.filteri (fun i _ -> i < max_slices - 1) slices in
    let rest = List.filteri (fun i _ -> i >= max_slices - 1) slices in
    let pct =
      List.fold_left
        (fun acc s -> match s.s_pct with Some p -> acc +. p | None -> acc)
        0. rest
    in
    keep
    @ [ { s_name = Printf.sprintf "Other (%d)" (List.length rest);
          s_pct = Some pct; s_value = "" } ]
  end

(* Largest-remainder apportionment, with a floor of one cell for any non-zero
   slice so a small holding never silently disappears from the bar. *)
let allocate_cells width slices =
  let pcts =
    Array.of_list (List.map (fun s -> Option.value s.s_pct ~default:0.) slices)
  in
  let n = Array.length pcts in
  let cells = Array.make n 0 in
  let total = Array.fold_left ( +. ) 0. pcts in
  if n = 0 || width <= 0 || total <= 0. then cells
  else begin
    let exact = Array.map (fun p -> p /. total *. float_of_int width) pcts in
    let floor_at_least_one = n <= width in
    Array.iteri
      (fun i e ->
         let b = int_of_float (floor e) in
         cells.(i) <- (if floor_at_least_one && pcts.(i) > 0. && b = 0 then 1 else b))
      exact;
    let used () = Array.fold_left ( + ) 0 cells in
    (* Hand out any leftover cells to the largest fractional remainders. *)
    let by_remainder =
      let idx = Array.init n (fun i -> i) in
      let rem i = exact.(i) -. floor exact.(i) in
      Array.sort (fun a b -> compare (rem b) (rem a)) idx;
      idx
    in
    let deficit = width - used () in
    for k = 0 to deficit - 1 do
      let i = by_remainder.(k mod n) in
      cells.(i) <- cells.(i) + 1
    done;
    (* The one-cell floor can overshoot; claw back from the widest slices. *)
    let rec claw budget =
      if used () > width && budget > 0 then begin
        let best = ref (-1) in
        Array.iteri
          (fun i c -> if c > 1 && (!best < 0 || c > cells.(!best)) then best := i)
          cells;
        if !best >= 0 then begin
          cells.(!best) <- cells.(!best) - 1;
          claw (budget - 1)
        end
      end
    in
    claw ((2 * width) + n);
    cells
  end

let bar_image width slices cells =
  let filled =
    List.mapi
      (fun i _slice ->
         let c = cells.(i) in
         if c <= 0 then I.empty
         else
           I.string
             series_attrs.(i mod Array.length series_attrs)
             (String.make c series_glyphs.(i mod max_slices)))
      slices
    |> I.hcat
  in
  let used = Array.fold_left ( + ) 0 cells in
  let rest =
    if used < width then I.string a_dim (String.make (width - used) '.') else I.empty
  in
  I.hcat [ I.string a_dim "["; filled; rest; I.string a_dim "]" ]

let legend_image slices =
  let name_w =
    List.fold_left (fun acc s -> max acc (String.length s.s_name)) 0 slices
  in
  let pct_text s =
    match s.s_pct with Some p -> fmt_share p | None -> "-"
  in
  let pct_w =
    List.fold_left (fun acc s -> max acc (String.length (pct_text s))) 0 slices
  in
  let value_w =
    List.fold_left (fun acc s -> max acc (String.length s.s_value)) 0 slices
  in
  List.mapi
    (fun i s ->
       I.hcat
         [ I.string A.empty "  ";
           (* The glyph carries identity; the text stays in normal ink. *)
           I.string
             series_attrs.(i mod Array.length series_attrs)
             (String.make 1 series_glyphs.(i mod max_slices));
           I.string A.empty " ";
           I.string a_label (pad `Left name_w s.s_name);
           I.string A.empty "  ";
           I.string
             (match s.s_pct with Some _ -> a_share | None -> a_dim)
             (pad `Right pct_w (pct_text s));
           I.string A.empty "  ";
           I.string a_dim (pad `Right value_w s.s_value) ])
    slices
  |> I.vcat

let chart ~title ~width slices =
  if slices = [] then I.empty
  else begin
    let slices = fold_extra slices in
    let bar_w = max 10 (min (width - 2) 60) in
    let cells = allocate_cells bar_w slices in
    I.vcat
      [ I.string a_header title;
        bar_image bar_w slices cells;
        legend_image slices;
        I.void 1 1 ]
  end

(* {1 Screen} *)

type view = Table | Charts

let rule width = I.string a_dim (String.make (max 0 width) '-')

let banner (p : Portfolio.t) =
  I.hcat
    [ I.string a_header "NET WORTH  "; I.string a_banner (fmt_usd p.total_usd) ]

let footer view inner (p : Portfolio.t) =
  let notes =
    (match p.price_error with
     | Some e -> [ I.string a_error ("prices: " ^ truncate (inner - 2) e) ]
     | None -> [])
    @ List.map
        (fun ((w : Config.wallet), msg) ->
           I.string a_error
             (truncate inner
                (Printf.sprintf "%s %s: %s" (Config.string_of_chain w.chain)
                   w.label msg)))
        (Portfolio.errors p)
  in
  let hint =
    match view with
    | Table -> "press c for charts, q or Ctrl-C to quit"
    | Charts -> "press c for the table, q or Ctrl-C to quit"
  in
  I.vcat (notes @ [ I.string a_dim hint ])

let render_table (p : Portfolio.t) =
  let rows = rows_of_portfolio p in
  let widths = column_widths rows in
  let table_width = Array.fold_left (fun acc w -> acc + w + 2) 0 widths in
  ( table_width,
    I.vcat
      [ render_header widths;
        rule table_width;
        I.vcat (List.map (render_row widths) rows) ] )

(* (1) each asset's share of net worth, then (2) for every asset that holds
   something, how that asset is split across its wallets. *)
let render_charts (p : Portfolio.t) width =
  let allocation =
    List.map
      (fun ((a : Portfolio.asset), pct) ->
         { s_name = Config.string_of_chain a.chain;
           s_pct = pct;
           s_value =
             (match a.total_usd with Some v -> fmt_usd v | None -> "unpriced") })
      (Portfolio.allocation p)
  in
  let per_asset =
    List.filter_map
      (fun (a : Portfolio.asset) ->
         if a.total_amount <= 0. then None
         else
           let slices =
             List.filter_map
               (fun (h : Portfolio.holding) ->
                  match h.amount with
                  | Ok amount ->
                    Some
                      { s_name = h.wallet.label;
                        s_pct = h.share;
                        s_value = fmt_amount amount }
                  | Error _ -> None)
               a.holdings
           in
           let name = Config.string_of_chain a.chain in
           Some
             (chart
                ~title:
                  (Printf.sprintf "%s BY WALLET (%% of %s held)" name name)
                ~width slices))
      p.assets
  in
  I.vcat
    (chart ~title:"ALLOCATION (% of net worth)" ~width allocation :: per_asset)

let render view (p : Portfolio.t) (width, _height) =
  let title = I.string a_title "  WALLET WATCHER  " in
  let body_width, body =
    match view with
    | Table -> render_table p
    | Charts -> (min (max width 40) 100, render_charts p (min (max width 40) 100))
  in
  let inner = max body_width (min width 100) in
  I.vcat
    [ title; banner p; rule inner; body; rule inner; footer view inner p ]
  |> I.pad ~l:1 ~t:1

(* {1 Event loop} *)

let is_quit = function
  | `Key (`ASCII 'q', _) | `Key (`Escape, _) -> true
  | `Key (`ASCII ('c' | 'C'), mods) -> List.mem `Ctrl mods
  | _ -> false

let is_toggle = function
  | `Key (`ASCII ('c' | 'C'), mods) -> not (List.mem `Ctrl mods)
  | _ -> false

let run (p : Portfolio.t) =
  let term = Notty_lwt.Term.create () in
  let view = ref Table in
  let draw () =
    Notty_lwt.Term.image term (render !view p (Notty_lwt.Term.size term))
  in
  let events = Notty_lwt.Term.events term in
  let rec loop () =
    Lwt_stream.get events >>= function
    | None -> Lwt.return_unit
    | Some event when is_quit event -> Lwt.return_unit
    | Some event when is_toggle event ->
      view := (match !view with Table -> Charts | Charts -> Table);
      draw () >>= loop
    (* Notty does not redraw on resize; re-render against the new size. *)
    | Some (`Resize _) -> draw () >>= loop
    | Some _ -> loop ()
  in
  draw () >>= loop >>= fun () -> Notty_lwt.Term.release term
