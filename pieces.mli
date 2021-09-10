(** The team the piece is on *)
type piece_team =
  | White
  | Black

(** The type of piece this is: pawn, bishop, king, queen, rook, or
    knight *)
type piece_type =
  | Pawn
  | Bishop
  | King
  | Queen
  | Rook
  | Knight

(** Type representing an individual chess piece *)
type piece = {
  team : piece_team;
  role : piece_type;
  moved : bool;
}

(** [get_piece_type p] returns [p]'s type *)
val get_piece_type : piece -> piece_type

val has_moved : piece -> bool

(** [get_piece_team p] returns [p]'s team *)
val get_piece_team : piece -> piece_team

(** [set_piece t r] returns a new piece with given parameters *)
val set_piece : piece_team -> piece_type -> bool -> piece

(** [piece_to_string p] returns the ASCII representation of the piece *)
val piece_to_string : piece -> string
