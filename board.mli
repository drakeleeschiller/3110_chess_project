open Pieces

(* [position (int * int)] represents the coordinates of the squares on a
   chess board, (col, row) *)
type position = int * int

(* [place (position * piece option)] is the combination of position and
   piece option *)
type tile = position * piece option

(* [t] is the abstract type of values representing the game state *)
type t = tile list

(* [new_board] creates a board and all pieces in their starting
   posiitons*)
val new_board : t
val blank_board : t
(** print_board prints a board in a standard chessboard configuration *)
val print_board : t -> unit
