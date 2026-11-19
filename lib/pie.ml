type cell = { glyph : Uchar.t; slice : int }

let braille_base = 0x2800
let half_dot_gap = 0.5
let two_pi = 2. *. Float.pi

let dot_bit ~dot_col ~dot_row =
  match (dot_col, dot_row) with
  | 0, 0 -> 0x01
  | 0, 1 -> 0x02
  | 0, 2 -> 0x04
  | 1, 0 -> 0x08
  | 1, 1 -> 0x10
  | 1, 2 -> 0x20
  | 0, _ -> 0x40
  | _ -> 0x80

(* Dot centres sit exactly half a dot off axis-aligned rays; the half-open
   interval and epsilon put the one-dot gap on the clockwise side every time. *)
let tie_epsilon = 1e-9

let is_on_gap ~angle dx dy =
  let along = (dx *. sin angle) -. (dy *. cos angle) in
  let clockwise_offset = (dx *. cos angle) +. (dy *. sin angle) in
  along > 0.
  && clockwise_offset > -.half_dot_gap +. tie_epsilon
  && clockwise_offset <= half_dot_gap +. tie_epsilon

let clockwise_fraction dx dy =
  let angle = Float.atan2 dx (-.dy) in
  (if angle < 0. then angle +. two_pi else angle) /. two_pi

let slice_of_fraction ~slice_ends ~last_non_empty ~is_non_empty fraction =
  let count = Array.length slice_ends in
  let rec find i =
    if i >= count then last_non_empty
    else if is_non_empty.(i) && slice_ends.(i) > fraction then i
    else find (i + 1)
  in
  find 0

let cells ~radius weights =
  if radius <= 0 then [||]
  else begin
    let rows = ((2 * radius) + 3) / 4 and cols = radius in
    let positive =
      Array.of_list (List.map (fun w -> if w > 0. then w else 0.) weights)
    in
    let total = Array.fold_left ( +. ) 0. positive in
    let result = Array.make_matrix rows cols None in
    if total <= 0. then result
    else begin
      let slice_count = Array.length positive in
      let is_non_empty = Array.map (fun w -> w > 0.) positive in
      let slice_ends =
        let running = ref 0. in
        Array.map
          (fun w ->
            running := !running +. (w /. total);
            !running)
          positive
      in
      let last_non_empty =
        let last = ref 0 in
        Array.iteri (fun i e -> if e then last := i) is_non_empty;
        !last
      in
      let boundary_angles =
        let non_empty_count =
          Array.fold_left (fun n e -> if e then n + 1 else n) 0 is_non_empty
        in
        if non_empty_count < 2 then []
        else
          List.filter_map
            (fun i ->
              if not is_non_empty.(i) then None
              else
                let start = if i = 0 then 0. else slice_ends.(i - 1) in
                Some (start *. two_pi))
            (List.init slice_count Fun.id)
      in
      let width = 2 * cols and height = 4 * rows in
      let centre = float_of_int radius in
      let owner = Array.make_matrix height width (-1) in
      let lit = Array.make_matrix height width false in
      let sector_dots = Array.make slice_count 0 in
      let lit_dots = Array.make slice_count 0 in
      for y = 0 to height - 1 do
        for x = 0 to width - 1 do
          let dx = float_of_int x +. 0.5 -. centre
          and dy = float_of_int y +. 0.5 -. centre in
          if (dx *. dx) +. (dy *. dy) <= centre *. centre then begin
            let slice =
              slice_of_fraction ~slice_ends ~last_non_empty ~is_non_empty
                (clockwise_fraction dx dy)
            in
            owner.(y).(x) <- slice;
            sector_dots.(slice) <- sector_dots.(slice) + 1;
            let on_gap =
              List.exists (fun angle -> is_on_gap ~angle dx dy) boundary_angles
            in
            if not on_gap then begin
              lit.(y).(x) <- true;
              lit_dots.(slice) <- lit_dots.(slice) + 1
            end
          end
        done
      done;
      let starved slice = sector_dots.(slice) > 0 && lit_dots.(slice) = 0 in
      for y = 0 to height - 1 do
        for x = 0 to width - 1 do
          if owner.(y).(x) >= 0 && starved owner.(y).(x) then lit.(y).(x) <- true
        done
      done;
      for row = 0 to rows - 1 do
        for col = 0 to cols - 1 do
          let mask = ref 0 in
          let votes = Array.make slice_count 0 in
          for dot_row = 0 to 3 do
            for dot_col = 0 to 1 do
              let y = (row * 4) + dot_row and x = (col * 2) + dot_col in
              if lit.(y).(x) then begin
                mask := !mask lor dot_bit ~dot_col ~dot_row;
                votes.(owner.(y).(x)) <- votes.(owner.(y).(x)) + 1
              end
            done
          done;
          if !mask <> 0 then begin
            let winner = ref 0 in
            Array.iteri (fun i v -> if v > votes.(!winner) then winner := i) votes;
            result.(row).(col) <-
              Some { glyph = Uchar.of_int (braille_base + !mask); slice = !winner }
          end
        done
      done;
      result
    end
  end
