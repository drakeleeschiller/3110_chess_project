open Board
open Moves
open Pieces

(* type status = | Ongoing | End *)
type t = {
  board : Board.t;
  prev_board : Board.t;
  turn : piece_team;
}

(** init_state is the initial state with the initial board and White's
    turn*)
val init_state : t

val set_state : Pieces.piece_team -> Board.t -> Board.t -> t

(** move_state is the state after a valid move is performed on the board
    of a given state from one position to another. Raises exception if
    the move puts the moving team in check, or if the state is
    checkmate, as well as all the exceptions in move_board*)
val move_state : t -> Board.position -> Board.position -> t

val next_turn : t -> Pieces.piece_team

val move_board : Board.t -> Board.position -> Board.position -> Board.t

val all_moves : t -> t list

(** is_checkmate is whether a current state is checkmate, that the
    current team has no valid moves that will get the king out of check *)
val is_checkmate : t -> bool

val is_stalemate : t -> bool

val team_tiles : Pieces.piece_team -> t -> Board.tile list

val insufficient_material : t -> bool

val castle : t -> bool -> t

val print_state : t -> unit

val newpiece : string -> Pieces.piece_type

val is_promotable : Pieces.piece option -> Board.position -> bool

val promotes : t -> Board.position -> Pieces.piece_type -> t
