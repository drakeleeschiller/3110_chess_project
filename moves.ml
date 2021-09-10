open Pieces

let diff_move pos1 pos2 = pos1 <> pos2

let valid_king_move pos1 pos2 =
  abs (fst pos2 - fst pos1) <= 1 && abs (snd pos2 - snd pos1) <= 1

let diagonal_move pos1 pos2 =
  abs (fst pos2 - fst pos1) = abs (snd pos2 - snd pos1)

let horizontal_move pos1 pos2 = snd pos2 = snd pos1

let vertical_move pos1 pos2 = fst pos2 = fst pos1

let valid_queen_move pos1 pos2 =
  diagonal_move pos1 pos2 || vertical_move pos1 pos2
  || horizontal_move pos1 pos2

let valid_bishop_move pos1 pos2 = diagonal_move pos1 pos2

let valid_rook_move pos1 pos2 =
  vertical_move pos1 pos2 || horizontal_move pos1 pos2

let valid_knight_move pos1 pos2 =
  (abs (fst pos2 - fst pos1) = 2 && abs (snd pos2 - snd pos1) = 1)
  || (abs (fst pos2 - fst pos1) = 1 && abs (snd pos2 - snd pos1) = 2)

(** Without capturing, but capturing with pawns is implemented in
    state.ml *)
let valid_pawn_move (p : Pieces.piece) pos1 pos2 =
  match get_piece_team p with
  | White ->
      if has_moved p then fst pos1 = fst pos2 && snd pos2 - snd pos1 = 1
      else
        fst pos1 = fst pos2
        && snd pos2 - snd pos1 <= 2
        && snd pos2 - snd pos1 > 0
  | Black ->
      if has_moved p then fst pos1 = fst pos2 && snd pos1 - snd pos2 = 1
      else
        fst pos1 = fst pos2
        && snd pos1 - snd pos2 <= 2
        && snd pos1 - snd pos2 > 0

let valid_norm_move p pos1 pos2 =
  diff_move pos1 pos2
  &&
  match get_piece_type p with
  | King -> valid_king_move pos1 pos2
  | Knight -> valid_knight_move pos1 pos2
  | Rook -> valid_rook_move pos1 pos2
  | Queen -> valid_queen_move pos1 pos2
  | Pawn -> valid_pawn_move p pos1 pos2
  | Bishop -> valid_bishop_move pos1 pos2
