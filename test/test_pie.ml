open Wallet_watcher

let failures = ref 0
let passes = ref 0

let check name ok =
  if ok then (incr passes; Printf.printf "  ok   %s\n" name)
  else (incr failures; Printf.printf "  FAIL %s\n" name)

let summary () =
  Printf.printf "\n%d passed, %d failed\n" !passes !failures;
  if !failures > 0 then exit 1

let lit_bits_of_glyph glyph = Uchar.to_int glyph - 0x2800

let dot_offsets =
  [ (0x01, 0, 0); (0x02, 0, 1); (0x04, 0, 2); (0x08, 1, 0);
    (0x10, 1, 1); (0x20, 1, 2); (0x40, 0, 3); (0x80, 1, 3) ]

(* Every lit dot as (dot_x, dot_y), decoded from the glyph bits. *)
let lit_dots grid =
  let dots = ref [] in
  Array.iteri
    (fun row cols ->
      Array.iteri
        (fun col cell ->
          Option.iter
            (fun { Pie.glyph; _ } ->
              let bits = lit_bits_of_glyph glyph in
              List.iter
                (fun (bit, dot_col, dot_row) ->
                  if bits land bit <> 0 then
                    dots := ((col * 2) + dot_col, (row * 4) + dot_row) :: !dots)
                dot_offsets)
            cell)
        cols)
    grid;
  !dots

let slice_of_dot ~radius ~weights (x, y) =
  let dx = float_of_int x +. 0.5 -. float_of_int radius
  and dy = float_of_int y +. 0.5 -. float_of_int radius in
  let angle = Float.atan2 dx (-.dy) in
  let fraction =
    (if angle < 0. then angle +. (2. *. Float.pi) else angle) /. (2. *. Float.pi)
  in
  let total = List.fold_left ( +. ) 0. weights in
  let rec find i running = function
    | [] -> i - 1
    | w :: rest ->
        let running = running +. (w /. total) in
        if w > 0. && running > fraction then i else find (i + 1) running rest
  in
  find 0 0. weights

let dots_per_slice ~radius ~weights grid =
  let counts = Array.make (List.length weights) 0 in
  List.iter
    (fun dot ->
      let slice = slice_of_dot ~radius ~weights dot in
      counts.(slice) <- counts.(slice) + 1)
    (lit_dots grid);
  counts

let all_none grid = Array.for_all (Array.for_all Option.is_none) grid

let max_share_error_points ~radius ~weights =
  let counts = dots_per_slice ~radius ~weights (Pie.cells ~radius weights) in
  let lit_total = float_of_int (Array.fold_left ( + ) 0 counts) in
  let weight_total = List.fold_left ( +. ) 0. weights in
  List.mapi
    (fun i w ->
      Float.abs
        ((100. *. float_of_int counts.(i) /. lit_total) -. (100. *. w /. weight_total)))
    weights
  |> List.fold_left Float.max 0.

let test_dimensions () =
  List.iter
    (fun radius ->
      let grid = Pie.cells ~radius [ 1. ] in
      let expect_rows = (2 * radius + 3) / 4 in
      check
        (Printf.sprintf "dimensions radius %d" radius)
        (Array.length grid = expect_rows
        && Array.for_all (fun row -> Array.length row = radius) grid))
    [ 1; 2; 3; 4; 5; 10; 15; 16 ];
  check "radius 0 is empty" (Pie.cells ~radius:0 [ 1. ] = [||]);
  check "negative radius is empty" (Pie.cells ~radius:(-3) [ 1. ] = [||])

let test_single_slice_is_disc () =
  let radius = 10 in
  let grid = Pie.cells ~radius [ 7. ] in
  let lit_cells_are_slice_zero =
    Array.for_all
      (Array.for_all (function Some { Pie.slice; _ } -> slice = 0 | None -> true))
      grid
  in
  check "single slice owns every lit cell" lit_cells_are_slice_zero;
  let is_full = function
    | Some { Pie.glyph; _ } -> Uchar.to_int glyph = 0x28FF
    | None -> false
  in
  let centre_row = radius / 4 and centre_col = radius / 2 in
  check "centre cells are full"
    (is_full grid.(centre_row).(centre_col - 1)
    && is_full grid.(centre_row).(centre_col)
    && is_full grid.(centre_row - 1).(centre_col));
  let dot_count = List.length (lit_dots grid) in
  let disc_area = Float.pi *. float_of_int (radius * radius) in
  check "single slice area is about pi r^2"
    (Float.abs (float_of_int dot_count -. disc_area) /. disc_area < 0.03)

let test_empty_inputs () =
  check "empty list is all None" (all_none (Pie.cells ~radius:8 []));
  check "empty list keeps dimensions"
    (Array.length (Pie.cells ~radius:8 []) = 4);
  check "all-zero is all None" (all_none (Pie.cells ~radius:8 [ 0.; 0.; 0. ]));
  check "negative weights are all None" (all_none (Pie.cells ~radius:8 [ -1.; -5. ]))

let test_proportionality () =
  let cases =
    [ [ 50.; 30.; 20. ]; [ 90.; 10. ]; [ 1.; 1.; 1.; 1. ]; [ 60.; 25.; 10.; 5. ] ]
  in
  List.iter
    (fun weights ->
      List.iter
        (fun radius ->
          let error = max_share_error_points ~radius ~weights in
          Printf.printf "    share error %5.2f pts  radius %2d  %s\n" error radius
            (String.concat ";" (List.map (Printf.sprintf "%g") weights));
          check
            (Printf.sprintf "shares within 3 points, radius %d, %d slices" radius
               (List.length weights))
            (error <= 3.))
        [ 12; 14; 16 ])
    cases

let test_zero_and_tiny_slices () =
  let weights = [ 50.; 0.; 50. ] in
  let counts = dots_per_slice ~radius:14 ~weights (Pie.cells ~radius:14 weights) in
  check "zero weight gets no dots" (counts.(1) = 0);
  check "neighbours of zero weight are lit" (counts.(0) > 0 && counts.(2) > 0);
  let weights = [ 99.; 1. ] in
  let counts = dots_per_slice ~radius:16 ~weights (Pie.cells ~radius:16 weights) in
  check "1% slice lights a dot" (counts.(1) > 0);
  let weights = [ 0.1; 99.9 ] in
  let counts = dots_per_slice ~radius:16 ~weights (Pie.cells ~radius:16 weights) in
  check "tiny slice never exceeds its sector"
    (counts.(0) <= 3 && counts.(1) > 0)

let test_boundaries_leave_gaps () =
  let radius = 14 in
  let with_gaps = List.length (lit_dots (Pie.cells ~radius [ 1.; 1. ])) in
  let without_gaps = List.length (lit_dots (Pie.cells ~radius [ 1. ])) in
  check "two slices lose dots to the gaps" (with_gaps < without_gaps)

let test_orientation () =
  let radius = 12 in
  let grid = Pie.cells ~radius [ 25.; 75. ] in
  let slice_at row col =
    Option.map (fun { Pie.slice; _ } -> slice) grid.(row).(col)
  in
  check "slice 0 sits in the top-right quadrant"
    (Array.for_all Fun.id
       (Array.mapi
          (fun row cols ->
            Array.for_all Fun.id
              (Array.mapi
                 (fun col cell ->
                   match cell with
                   | Some { Pie.slice = 0; _ } ->
                       col * 2 >= radius && (row + 1) * 4 <= radius
                   | _ -> true)
                 cols))
          grid));
  check "top-right cell is slice 0" (slice_at 1 10 = Some 0);
  check "top-left cell is slice 1" (slice_at 1 3 = Some 1);
  check "bottom-right cell is slice 1" (slice_at 4 10 = Some 1);
  check "bottom-left cell is slice 1" (slice_at 4 3 = Some 1)

let test_slice_order_clockwise () =
  let radius = 12 in
  let grid = Pie.cells ~radius [ 25.; 25.; 25.; 25. ] in
  let slice_at row col =
    Option.map (fun { Pie.slice; _ } -> slice) grid.(row).(col)
  in
  check "quadrants run clockwise from 12"
    (slice_at 1 10 = Some 0
    && slice_at 4 10 = Some 1
    && slice_at 4 3 = Some 2
    && slice_at 1 3 = Some 3)

let visible_slices grid =
  Array.fold_left
    (Array.fold_left (fun acc -> function
       | Some { Pie.slice; _ } -> if List.mem slice acc then acc else slice :: acc
       | None -> acc))
    [] grid

let test_tiny_slice_is_visible () =
  List.iter
    (fun (radius, weights) ->
      let shown = visible_slices (Pie.cells ~radius weights) in
      check
        (Printf.sprintf "every positive weight shows at radius %d" radius)
        (List.for_all (fun i -> List.mem i shown)
           (List.filteri (fun i _ -> List.nth weights i > 0.) (List.mapi (fun i _ -> i) weights))))
    [ (8, [ 99.5; 0.5 ]); (8, [ 0.5; 99.5 ]); (8, [ 50.; 0.2; 49.8 ]);
      (16, [ 99.9; 0.1 ]); (2, [ 99.5; 0.5 ]) ];
  check "rows and cols helpers match the grid"
    (let grid = Pie.cells ~radius:9 [ 1.; 2. ] in
     Array.length grid = Pie.rows ~radius:9 && Array.length grid.(0) = Pie.cols ~radius:9)

let () =
  test_dimensions ();
  test_tiny_slice_is_visible ();
  test_single_slice_is_disc ();
  test_empty_inputs ();
  test_proportionality ();
  test_zero_and_tiny_slices ();
  test_boundaries_leave_gaps ();
  test_orientation ();
  test_slice_order_clockwise ();
  summary ()
