open OUnit2
open Board
open Pieces
open Moves
open State

let moves_test name move pos1 pos2 output =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal output (move pos1 pos2)

let pieces_test name accessor input output =
  name >:: fun _ -> assert_equal output (accessor input)

let wpawn = set_piece White Pawn false

let bpawn = set_piece Black Pawn false

let wqueen = set_piece White Queen false

let wpawn = set_piece White Pawn false

let bpawn = set_piece Black Pawn false

let wrook = set_piece White Rook false

let wbishop = set_piece White Bishop false

let wknight = set_piece White Knight false

let wqueen = set_piece White Queen false

let wking = set_piece White King false

let moves_tests =
  [
    moves_test "queen move" valid_queen_move (1, 1) (8, 8) true;
    moves_test "king move" valid_king_move (3, 2) (3, 3) true;
    moves_test "wpawn move" (valid_pawn_move wpawn) (1, 2) (1, 3) true;
    moves_test "norm move queen"
      (valid_norm_move wqueen)
      (5, 5) (5, 8) true;
    moves_test "norm move knight"
      (valid_norm_move wknight)
      (2, 1) (3, 3) true;
    moves_test "wpawn 2 spaces" (valid_norm_move bpawn) (1, 7) (1, 5)
      true;
    moves_test "same square" (valid_norm_move wking) (1, 1) (1, 1) false;
  ]

let pieces_tests =
  [
    pieces_test "Team White Test" get_piece_team wpawn White;
    pieces_test "Team Black Test" get_piece_team bpawn Black;
    pieces_test "Pawn Test" get_piece_type bpawn Pawn;
    pieces_test "Rook Test" get_piece_type wrook Rook;
    pieces_test "Bishop Test" get_piece_type wbishop Bishop;
    pieces_test "Knight Test" get_piece_type wknight Knight;
    pieces_test "King Test" get_piece_type wking King;
    pieces_test "Queen Test" get_piece_type wqueen Queen;
  ]

let wrook = set_piece White Rook false

let number_of (lst : Board.t) opt =
  lst |> List.map snd |> List.filter opt |> List.length

let board_tests =
  [
    (* checks if there's a white rook in the bottom right corner *)
    ( "corner rook" >:: fun _ ->
      assert_equal (List.mem ((1, 1), Some wrook) new_board) true );
    (* checks the length of the tile list // should always be 64 *)
    ( "length tile list" >:: fun _ ->
      assert_equal (List.length new_board) 64 );
    (* checks the number of Some pieces in the initial state*)
    ( "number of Some pieces" >:: fun _ ->
      assert_equal (number_of new_board Option.is_some) 32 );
    (* checks the number of None options in the initial state*)
    ( "number of None's" >:: fun _ ->
      assert_equal (number_of new_board Option.is_none) 32 );
  ]

(**Checks that after a piece has been moved from a start position to an
   end position that the piece moved is actually in the end position in
   the new board*)
let state_test name board start endpos =
  let new_board = State.move_board board start endpos in
  let p = List.assoc start board in
  name >:: fun _ -> assert_equal (List.assoc endpos new_board) p

(**Makes sure an exception is raised for any illegal moves inputted*)
let state_test_exc name board start endpos exc =
  name >:: fun _ ->
  assert_raises exc (fun () -> State.move_board board start endpos)

(* To-Do: Finish state_tests *)
let state_tests =
  [
    state_test "Move a pawn one space" new_board (1, 2) (1, 3);
    state_test "Move a pawn two spaces" new_board (1, 2) (1, 4);
    state_test "Move a Knight to the right" new_board (2, 1) (3, 3);
    state_test "Move a Knight to the left" new_board (2, 1) (1, 3);
    state_test_exc "Move a Rook over a Pawn" new_board (1, 1) (1, 4)
      (Invalid_argument "Invalid Move for this Piece");
    state_test_exc "Move a Knight straight" new_board (2, 1) (2, 3)
      (Invalid_argument "Invalid Move for this Piece");
    state_test_exc "Move a Bishop through something" new_board (3, 1)
      (2, 3) (Invalid_argument "Invalid Move for this Piece");
    (*state_test_exc "Move a Pawn backwards" (move (new_board) (1, 2)
      (1, 4)) (1, 4) (1, 3) (Invalid_argument "Invalid Move for this
      Piece");*)
    state_test_exc "Move a Queen through something" new_board (4, 1)
      (4, 3) (Invalid_argument "Invalid Move for this Piece");
    state_test_exc "Move a King through something" new_board (5, 1)
      (5, 3) (Invalid_argument "Invalid Move for this Piece");
    state_test_exc "Move a non existent piece" new_board (5, 3) (5, 4)
      (Invalid_argument "No Piece on Position");
    state_test "Pawn captures Pawn"
      (State.move_board
         (State.move_board new_board (1, 2) (1, 4))
         (2, 7) (2, 5))
      (1, 4) (2, 5);
  ]

let tests =
  "test suite for proj"
  >::: List.flatten
         [ moves_tests; pieces_tests; board_tests; state_tests ]

let _ = run_test_tt_main tests
