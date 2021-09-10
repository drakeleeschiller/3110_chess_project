open Pieces
(** diff_move is whether two int*int positions are unequal*)
val diff_move : int * int -> int * int -> bool

(** diagonal_move is whether two int*int positions are in a diagonal path *)
val diagonal_move : int * int -> int * int -> bool

(** horizontal_move is whether two int*int positions are in a horizontal path*)
val horizontal_move : int * int -> int * int -> bool

(** vertical_move is whether two int*int positions are in a vertical path*)
val vertical_move : int * int -> int * int -> bool

(** valid_king_move is whether two int*int positions represent a valid basic king move*)
val valid_king_move : int * int -> int * int -> bool

(** valid_queen_move is whether two int*int positions represent a valid basic queen move*)
val valid_queen_move : int * int -> int * int -> bool

(** valid_bishop_move is whether two int*int positions represent a valid basic bishop move*)
val valid_bishop_move : int * int -> int * int -> bool

(** valid_rook_move is whether two int*int positions represent a valid basic rook move*)
val valid_rook_move : int * int -> int * int -> bool

(** valid_knight_move is whether two int*int positions represent a valid basic knight move*)
val valid_knight_move : int * int -> int * int -> bool

(** valid_pawn_move is whether two int*int positions represent a valid basic pawn move given 
a pawn with its assigned team*)
val valid_pawn_move : piece -> int * int -> int * int -> bool

(** valid_norm_move is whether two int*int positions represent a valid basic move for the given piece*)
val valid_norm_move : piece -> int * int -> int * int -> bool
