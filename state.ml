open Board
open Moves
open Pieces

(* find_pos finds the position associated with a tile*)
type t = {
  board : Board.t;
  prev_board : Board.t;
  turn : piece_team;
}

(** next_turn returns the team that will go next*)
let next_turn st = match st.turn with White -> Black | Black -> White

(** init_state is the initial state with the initial board and White's
    turn*)
let init_state = { board = new_board; prev_board = []; turn = White }

(** set_state team board is the state with given board and team*)
let set_state team pboard board =
  { board; prev_board = pboard; turn = team }

(** find_pos finds the position associated with a tile*)
let find_pos (t : Board.tile) = fst t

(* find_piece finds the piece option associated with a tile*)
let find_piece (t : Board.tile) = snd t

(** no_team_piece_in_end_pos is whether there exists a piece of the same
    team in a move's end position for a given board and piece*)
let no_team_piece_in_end_pos board start_piece end_pos =
  let end_piece_opt = List.assoc end_pos board in
  match end_piece_opt with
  | Some p -> get_piece_team start_piece <> get_piece_team p
  | None -> true

(** right_move is whether the move from one position to another is in
    the rightward direction*)
let right_move pos1 pos2 = fst pos1 - fst pos2 < 0

(** left_move is whether the move from one position to another is in the
    leftward direction*)
let left_move pos1 pos2 = fst pos1 - fst pos2 > 0

(** up_move is whether the move from one position to another is a in the
    upward direction*)
let up_move pos1 pos2 = snd pos1 - snd pos2 < 0

(** piece_search_in_path is equal to if there is a piece on the board
    in-between two positions, with the direction of the path given by
    x_inc and y_inc *)
let rec piece_search_in_path board pos1 pos2 x_inc y_inc =
  let next_pos = (fst pos1 + x_inc, snd pos1 + y_inc) in
  if next_pos = pos2 then true
  else
    List.assoc next_pos board = None
    && piece_search_in_path board next_pos pos2 x_inc y_inc

(** diagonal_piece_search is equal to if there is a piece on the board
    between two positions in a diagonal path*)
let diagonal_piece_search board pos1 pos2 =
  if up_move pos1 pos2 then
    if right_move pos1 pos2 then
      piece_search_in_path board pos1 pos2 1 1
    else piece_search_in_path board pos1 pos2 (-1) 1
  else if right_move pos1 pos2 then
    piece_search_in_path board pos1 pos2 1 (-1)
  else piece_search_in_path board pos1 pos2 (-1) (-1)

(** diagonal_piece_search is equal to if there is a piece on the board
    between two positions in a horizontal or vertical path*)
let hor_or_vert_piece_search board pos1 pos2 =
  if up_move pos1 pos2 then piece_search_in_path board pos1 pos2 0 1
  else if right_move pos1 pos2 then
    piece_search_in_path board pos1 pos2 1 0
  else if left_move pos1 pos2 then
    piece_search_in_path board pos1 pos2 (-1) 0
  else piece_search_in_path board pos1 pos2 0 (-1)

(** no_piece_blocking is equal to if there is a piece blocking in the
    path between 2 positions, assuming that the positions represent a
    valid move for a piece*)
let no_piece_blocking (board : Board.t) pos1 pos2 =
  match List.assoc pos1 board with
  | None -> raise (Invalid_argument "No Piece Error")
  | Some p -> (
      match get_piece_type p with
      | Bishop -> diagonal_piece_search board pos1 pos2
      | Rook -> hor_or_vert_piece_search board pos1 pos2
      | Queen ->
          if diagonal_move pos1 pos2 then
            diagonal_piece_search board pos1 pos2
          else hor_or_vert_piece_search board pos1 pos2
      | Pawn ->
          if fst pos1 = fst pos2 then
            hor_or_vert_piece_search board pos1 pos2
          else true
      | _ -> true)

(** piece_in_end_pos is whether there exists a piece in a given position
    in a given board*)
let piece_in_end_pos board pos2 =
  match List.assoc pos2 board with None -> false | Some p -> true

(** valid_pawn_move_or_capture is whether a move for a pawn on a given
    board is a valid pawn move or capture*)
let valid_pawn_move_or_capture board p pos1 pos2 =
  if get_piece_type p = Pawn then
    let basic_move = valid_pawn_move p pos1 pos2 in
    let piece_in_end = piece_in_end_pos board pos2 in
    if basic_move && piece_in_end then false
    else if basic_move && not piece_in_end then true
    else
      match get_piece_team p with
      | White ->
          snd pos2 - snd pos1 = 1
          && abs (fst pos1 - fst pos2) = 1
          && piece_in_end
      | Black ->
          snd pos1 - snd pos2 = 1
          && abs (fst pos1 - fst pos2) = 1
          && piece_in_end
  else false

(** valid_move is whether the move from a position to another is valid
    on a given board (not considering being checked or en passant) *)
let valid_move board pos1 pos2 =
  match List.assoc pos1 board with
  | None -> false
  | Some p ->
      let valid_basic_move =
        if get_piece_type p = Pawn then
          valid_pawn_move_or_capture board p pos1 pos2
        else valid_norm_move p pos1 pos2
      in
      valid_basic_move
      && no_team_piece_in_end_pos board p pos2
      && no_piece_blocking board pos1 pos2

(** move is the board after a valid basic chess move is completed
    between two positions (not considering being checked or en passant)*)
let move_board
    (board : Board.t)
    (start_pos : Board.position)
    (end_pos : Board.position) =
  let start_piece = List.assoc start_pos board in
  let new_start_tile = (start_pos, None) in
  match start_piece with
  | None -> raise (Invalid_argument "No Piece on Position")
  | Some p ->
      if not (valid_move board start_pos end_pos) then
        raise (Invalid_argument "Invalid Move for this Piece")
      else
        let new_end_tile = (end_pos, Some { p with moved = true }) in
        let removed =
          board
          |> List.remove_assoc start_pos
          |> List.remove_assoc end_pos
        in
        new_start_tile :: new_end_tile :: removed

(** get_king_pos returns the position of a given team's king from a
    given board*)
let get_king_pos team board =
  board
  |> List.find (fun (_, popt) ->
         match popt with
         | None -> false
         | Some p -> get_piece_team p = team && get_piece_type p = King)
  |> fst

(** team_in_check is whether a given team is currently in check or not *)
let team_in_check team board =
  List.exists
    (fun (pos, _) -> board |> get_king_pos team |> valid_move board pos)
    board

(** is_en_passant is whether a move is a valid en passant capture on the
    current state*)
let is_en_passant st pos1 (x, y) =
  try
    let attacking_pawn = Option.get (List.assoc pos1 st.board) in
    if attacking_pawn.role = Pawn then
      match attacking_pawn.team with
      | White ->
          let captured_pawn =
            Option.get (List.assoc (x, y - 1) st.board)
          in
          captured_pawn.role = Pawn
          && captured_pawn.team = Black
          && snd pos1 = 5
          && y = 6
          && abs (fst pos1 - x) = 1
          && Option.is_none (List.assoc (x, y) st.board)
          && Option.is_none (List.assoc (x, y + 1) st.board)
          && (Option.get (List.assoc (x, y + 1) st.prev_board)).moved
             = false
      | Black ->
          let captured_pawn =
            Option.get (List.assoc (x, y + 1) st.board)
          in
          captured_pawn.role = Pawn
          && captured_pawn.team = White
          && snd pos1 = 4
          && y = 3
          && abs (fst pos1 - x) = 1
          && Option.is_none (List.assoc (x, y) st.board)
          && Option.is_none (List.assoc (x, y - 1) st.board)
          && (Option.get (List.assoc (x, y - 1) st.prev_board)).moved
             = false
    else false
  with _ -> false

(** en_passant_capture returns the new state after an en passant capture
    is performed*)
let en_passant_capture st pos1 (x, y) =
  let start_piece = Option.get (List.assoc pos1 st.board) in
  match start_piece.team with
  | White ->
      let next_state =
        (pos1, None)
        :: ((x, y - 1), None)
        :: ((x, y), List.assoc pos1 st.board)
        :: (st.board |> List.remove_assoc pos1
           |> List.remove_assoc (x, y - 1)
           |> List.remove_assoc (x, y))
      in
      set_state (next_turn st) st.board next_state
  | Black ->
      let next_state =
        (pos1, None)
        :: ((x, y + 1), None)
        :: ((x, y), List.assoc pos1 st.board)
        :: (st.board |> List.remove_assoc pos1
           |> List.remove_assoc (x, y + 1)
           |> List.remove_assoc (x, y))
      in
      set_state (next_turn st) st.board next_state

(** team_tiles returns all the pieces and positions of the given team in
    a given state*)
let team_tiles team st =
  let board = st.board in
  List.filter
    (fun t ->
      match find_piece t with
      | Some p -> get_piece_team p = team
      | None -> false)
    board

(**all_valid_moves is the list of all states that could result from a
   valid move from a piece of the current team in a given state,
   accounting for being checked but not for castling*)
let all_valid_basic_moves st =
  let board = st.board in
  let rec all_states_from_piece piece_lst =
    match piece_lst with
    | [] -> []
    | h :: t ->
        let s_pos = find_pos h in
        List.filter
          (fun p ->
            valid_move board s_pos p || is_en_passant st s_pos p)
          (board |> List.split |> fst)
        |> List.map (fun pos ->
               if is_en_passant st s_pos pos then
                 en_passant_capture st s_pos pos
               else
                 set_state (next_turn st) st.board
                   (move_board board s_pos pos))
        |> ( @ ) (all_states_from_piece t)
  in
  st |> team_tiles st.turn |> all_states_from_piece
  |> List.filter (fun s -> not (team_in_check st.turn s.board))

exception Checkmate

exception Draw of string

(** is_checkmate is whether a current state is checkmate, that the
    current team has no valid moves that will get the king out of check *)
let is_checkmate st =
  team_in_check st.turn st.board && all_valid_basic_moves st = []

let is_stalemate st =
  (not (team_in_check st.turn st.board))
  && all_valid_basic_moves st = []

let insufficient_material st =
  let all_pieces =
    team_tiles st.turn st @ team_tiles (next_turn st) st
    |> List.split |> snd |> List.map Option.get
    |> List.map get_piece_type
  in
  if List.length all_pieces <= 3 then
    match
      List.filter
        (fun pt -> not (pt = King || pt = Bishop || pt = Knight))
        all_pieces
    with
    | [] -> true
    | _ -> false
  else false

let is_promotable p (pos : position) =
  match p with
  | None -> false
  | Some p -> (
      if get_piece_type p <> Pawn then false
      else if get_piece_team p = White then
        match pos with _, 8 -> true | _ -> false
      else match pos with _, 1 -> true | _ -> false)

let newpiece p =
  if String.lowercase_ascii p = "bishop" then Bishop
  else if String.lowercase_ascii p = "rook" then Rook
  else if String.lowercase_ascii p = "knight" then Knight
  else if String.lowercase_ascii p = "queen" then Queen
  else raise (Invalid_argument "Invalid Piece Name")

let promotes st pos ptype =
  let p = List.assoc pos st.board in
  let l = st.board |> List.remove_assoc pos in
  match p with
  | None -> raise (Invalid_argument "Piece Not Promotable")
  | Some p ->
      set_state st.turn st.board
        ((pos, Some (set_piece (get_piece_team p) ptype true)) :: l)

(** move_state is the state after a valid move is performed on the board
    of a given state from one position to another. Doesn't include
    castling and promoting. Raises exception if the move puts the moving
    team in check, or if the state is checkmate, as well as all the
    exceptions in move_board *)
let move_state st pos1 pos2 =
  if
    match List.assoc pos1 st.board with
    | None -> raise (Invalid_argument "No Piece in Position")
    | Some p -> get_piece_team p <> st.turn
  then raise (Invalid_argument "No team piece in position")
  else
    let next_state =
      if is_en_passant st pos1 pos2 then en_passant_capture st pos1 pos2
      else
        set_state (next_turn st) st.board
          (move_board st.board pos1 pos2)
    in
    if is_checkmate st then raise Checkmate
    else if is_stalemate st then raise (Draw "Stalemate")
    else if team_in_check st.turn next_state.board then
      raise (Invalid_argument "Invalid Move, in Check")
    else next_state

(** king_hasnt_moved is whether a given team's king has not moved on a
    given board*)
let king_hasnt_moved board team =
  match List.assoc (get_king_pos team board) board with
  | None -> failwith "no king wut?"
  | Some p -> not (has_moved p)

(** left_rook_hasnt_moved is whether a given team's left rook hasn't
    moved on a given board*)
let left_rook_hasnt_moved board team =
  match team with
  | White -> (
      match List.assoc (1, 1) board with
      | None -> false
      | Some p -> get_piece_type p = Rook && has_moved p = false)
  | Black -> (
      match List.assoc (1, 8) board with
      | None -> false
      | Some p -> get_piece_type p = Rook && has_moved p = false)

(** right_rook_hasnt_moved is whether a given team's right rook hasn't
    moved on a given board*)
let right_rook_hasnt_moved board team =
  match team with
  | White -> (
      match List.assoc (8, 1) board with
      | None -> false
      | Some p -> get_piece_type p = Rook && has_moved p = false)
  | Black -> (
      match List.assoc (8, 8) board with
      | None -> false
      | Some p -> get_piece_type p = Rook && has_moved p = false)

(** change_board returns the new board after replacing pos2 with the
    piece on pos1 (used for castling)*)
let change_board board pos1 pos2 =
  let p = List.assoc pos1 board in
  board
  |> List.filter (fun t -> fst t = pos1 || fst t = pos2)
  |> ( @ ) [ (pos1, None); (pos2, p) ]

(** path_clear_of_check is whether a path to castle is clear of check or
    not in a given state *)
let rec path_clear_of_check st king_pos path =
  if team_in_check st.turn st.board then false
  else
    match path with
    | [] -> true
    | h :: t ->
        if team_in_check st.turn (change_board st.board king_pos h) then
          false
        else path_clear_of_check st king_pos t

(** can_castle_left is if the current team can castle left in a given
    state*)
let can_castle_left st =
  king_hasnt_moved st.board st.turn
  && left_rook_hasnt_moved st.board st.turn
  &&
  match st.turn with
  | White ->
      let castle_path = [ (3, 1); (4, 1) ] in
      no_piece_blocking st.board (1, 1) (5, 1)
      && path_clear_of_check st (5, 1) castle_path
  | Black ->
      let castle_path = [ (3, 8); (4, 8) ] in
      no_piece_blocking st.board (1, 8) (5, 8)
      && path_clear_of_check st (5, 8) castle_path

(** can_castle right is whether the current team can castle right in a
    given state *)
let can_castle_right st =
  king_hasnt_moved st.board st.turn
  && right_rook_hasnt_moved st.board st.turn
  &&
  match st.turn with
  | White ->
      let castle_path = [ (6, 1); (7, 1) ] in
      no_piece_blocking st.board (8, 1) (5, 1)
      && path_clear_of_check st (5, 1) castle_path
  | Black ->
      let castle_path = [ (6, 8); (7, 8) ] in
      no_piece_blocking st.board (8, 8) (5, 8)
      && path_clear_of_check st (5, 8) castle_path

(** castle is the state after a castle move is performed on a state,
    king_side is true if the player wants a king side castle or a queen
    side castle*)
let castle st king_side =
  let board =
    match st.turn with
    | White ->
        if king_side && can_castle_right st then
          st.board
          |> List.filter (fun t ->
                 not
                   (fst t = (5, 1)
                   || fst t = (6, 1)
                   || fst t = (7, 1)
                   || fst t = (8, 1)))
          |> ( @ )
               [
                 ((5, 1), None);
                 ( (6, 1),
                   Some { team = White; role = Rook; moved = true } );
                 ( (7, 1),
                   Some { team = White; role = King; moved = true } );
                 ((8, 1), None);
               ]
        else if (not king_side) && can_castle_left st then
          st.board
          |> List.filter (fun t ->
                 not
                   (fst t = (5, 1)
                   || fst t = (4, 1)
                   || fst t = (3, 1)
                   || fst t = (2, 1)
                   || fst t = (1, 1)))
          |> ( @ )
               [
                 ((5, 1), None);
                 ( (4, 1),
                   Some { team = White; role = Rook; moved = true } );
                 ( (3, 1),
                   Some { team = White; role = King; moved = true } );
                 ((2, 1), None);
                 ((1, 1), None);
               ]
        else raise (Invalid_argument "Invalid Castle")
    | Black ->
        if king_side && can_castle_right st then
          st.board
          |> List.filter (fun t ->
                 not
                   (fst t = (5, 8)
                   || fst t = (6, 8)
                   || fst t = (7, 8)
                   || fst t = (8, 8)))
          |> ( @ )
               [
                 ((5, 8), None);
                 ( (6, 8),
                   Some { team = Black; role = Rook; moved = true } );
                 ( (7, 8),
                   Some { team = Black; role = King; moved = true } );
                 ((8, 8), None);
               ]
        else if (not king_side) && can_castle_left st then
          st.board
          |> List.filter (fun t ->
                 not
                   (fst t = (5, 8)
                   || fst t = (4, 8)
                   || fst t = (3, 8)
                   || fst t = (2, 8)
                   || fst t = (1, 8)))
          |> ( @ )
               [
                 ((5, 8), None);
                 ( (4, 8),
                   Some { team = Black; role = Rook; moved = true } );
                 ( (3, 8),
                   Some { team = Black; role = King; moved = true } );
                 ((2, 8), None);
                 ((1, 8), None);
               ]
        else raise (Invalid_argument "Invalid Castle")
  in
  set_state (next_turn st) st.board board

let can_promote st =
  List.exists (fun t -> is_promotable (snd t) (fst t)) st.board

let promote_state st ptype =
  match
    st.board
    |> List.filter (fun tile -> is_promotable (snd tile) (fst tile))
  with
  | [] -> st
  | h :: t -> promotes st (fst h) ptype

let all_moves st =
  let basic_moves = all_valid_basic_moves st in
  let castles =
    (try [ castle st true ] with Invalid_argument a -> [])
    @ try [ castle st false ] with Invalid_argument a -> []
  in
  let promoting_move = basic_moves |> List.filter can_promote in
  let promotions =
    match promoting_move with
    | [] -> []
    | h :: t ->
        [
          promote_state h Queen;
          promote_state h Knight;
          promote_state h Bishop;
          promote_state h Rook;
        ]
  in
  promotions @ castles
  @ List.filter (fun s -> not (List.mem s promoting_move)) basic_moves

(** print_state prints the board and current turn of a given state *)
let print_state st =
  print_board st.board;
  match st.turn with
  | White -> print_endline "White's Turn"
  | Black -> print_endline "Black's Turn"
