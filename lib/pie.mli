(** Braille pie geometry, independent of any terminal library. *)

type cell = {
  glyph : Uchar.t; (** a braille pattern, U+2800..U+28FF *)
  slice : int; (** index into the input list of the majority lit dot *)
}

val rows : radius:int -> int
(** Number of cell rows [cells] returns for [radius]. *)

val cols : radius:int -> int
(** Number of cell columns [cells] returns for [radius]. *)

val cells : radius:int -> float list -> cell option array array
(** [cells ~radius weights] rasterises a pie of [radius] braille dots.
    Rows = ceil (2*radius / 4), columns = ceil (2*radius / 2); result is
    indexed [row].(col). Weights are normalised; zero or negative weights get
    no area. Slices start at 12 o'clock and run clockwise in list order.
    [None] is a cell with no lit dot. *)
