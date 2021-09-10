open Pieces
open Board
open State
open Ai

let print_moves st = List.iter print_state (all_moves st)

let print_eval (st, eval) =
  print_state st;
  print_int eval

let rec print_tree tree =
  match tree with
  | T (n, []) -> print_eval n
  | T (n, l) -> List.iter print_tree l

let insufficient_board =
  ((1, 8), Some (set_piece Black King true))
  :: ((4, 5), Some (set_piece White King true))
  :: ((3, 7), Some (set_piece White Bishop true))
  :: (blank_board
     |> List.remove_assoc (1, 8)
     |> List.remove_assoc (4, 5)
     |> List.remove_assoc (8, 7))
  |> set_state Black []

let promote_castle_board =
  ((8, 8), Some (set_piece Black King true))
  :: ((5, 1), Some (set_piece White King false))
  :: ((5, 5), Some (set_piece White Pawn true))
  :: ((6, 6), Some (set_piece Black Knight true))
  :: (blank_board
     |> List.remove_assoc (8, 8)
     |> List.remove_assoc (5, 1)
     |> List.remove_assoc (5, 5)
     |> List.remove_assoc (6, 6))
  |> set_state White []

let en_passant_board =
  ((8, 8), Some (set_piece Black King true))
  :: ((5, 1), Some (set_piece White King false))
  :: ((2, 5), Some (set_piece White Pawn true))
  :: ((3, 5), Some (set_piece Black Pawn true))
  :: (blank_board
     |> List.remove_assoc (8, 8)
     |> List.remove_assoc (5, 1)
     |> List.remove_assoc (2, 5)
     |> List.remove_assoc (3, 5))
  |> set_state White
       (((8, 8), Some (set_piece Black King true))
       :: ((5, 1), Some (set_piece White King false))
       :: ((2, 5), Some (set_piece White Pawn true))
       :: ((3, 7), Some (set_piece Black Pawn false))
       :: (blank_board
          |> List.remove_assoc (8, 8)
          |> List.remove_assoc (5, 1)
          |> List.remove_assoc (2, 5)
          |> List.remove_assoc (3, 7)))

let init_tree = moves_tree en_passant_board 1
