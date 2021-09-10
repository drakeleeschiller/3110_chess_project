type piece_team =
  | White
  | Black

type piece_type =
  | Pawn
  | Bishop
  | King
  | Queen
  | Rook
  | Knight

type piece = {
  team : piece_team;
  role : piece_type;
  moved : bool;
}

let has_moved p = p.moved

let get_piece_type p = p.role

let get_piece_team p = p.team

let set_piece t r m = { team = t; role = r; moved = m }

let piece_to_string p =
  match (p.team, p.role) with
  | Black, Pawn -> "♙"
  | Black, Queen -> "♕"
  | Black, King -> "♔"
  | Black, Bishop -> "♗"
  | Black, Knight -> "♘"
  | Black, Rook -> "♖"
  | White, Pawn -> "♟︎"
  | White, Queen -> "♛"
  | White, King -> "♚"
  | White, Bishop -> "♝"
  | White, Knight -> "♞"
  | White, Rook -> "♜"
