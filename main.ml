open Pieces
open State
open Board
open Command
open Ai

exception Checkmate

exception Draw of string

(* state takes in (s : State.t) (b : Board.t) (desc : bool). It should
   take in these arguments and return a call to itself if the read_line
   input is of type Go lst or Castle string, and but quit if it's of
   type Quit *)

let rules str =
  print_endline "Here are the rules: ";
  print_endline "1. All standard chess rules for gameplay apply.";
  print_endline
    "2. To move, type 'go' with the position of the piece you want to \
     move, and the position you want to move that piece to.";
  print_endline
    "   a. For example, 'go a2 a4' moves the pawn on square a2 to \
     square a4.";
  print_endline "3. To quit, just type 'quit' at any time.";
  print_endline
    "4. To access these rules again, just type 'rules' at any time."

let rec state (s : State.t) (move : bool) (num_players : int) =
  try
    if num_players = 0 || (num_players = 1 && s.turn = Black) then (
      print_state s;
      let next_move = optimal_move s in
      if is_checkmate next_move then (
        print_board next_move.board;
        raise Checkmate)
      else if is_stalemate next_move then (
        print_board next_move.board;
        raise (Draw "Stalemate"))
      else if insufficient_material next_move then (
        print_board next_move.board;
        raise (Draw "Insufficient Material"))
      else state next_move true num_players)
    else if move then (
      print_state s;
      print_newline ();
      print_endline "Please enter a new command.");
    print_string ">";
    let input = read_line () in
    match parse input with
    | Go lst -> (
        match lst with
        | [] ->
            raise
              (Malformed
                 "Please enter two positions after the [go] command.")
        | h :: t -> (
            if t = [] then
              raise (Malformed "Please enter a second position.")
            else
              let first_pos = str_to_pos h in
              match t with
              | h2 :: t2 ->
                  let second_pos = str_to_pos h2 in
                  let next_state = move_state s first_pos second_pos in
                  if is_checkmate next_state then (
                    print_board next_state.board;
                    raise Checkmate)
                  else if is_stalemate next_state then (
                    print_board next_state.board;
                    raise (Draw "Stalemate"))
                  else if insufficient_material next_state then (
                    print_board next_state.board;
                    raise (Draw "Insufficient Material"))
                  else if
                    is_promotable
                      (List.assoc second_pos next_state.board)
                      second_pos
                  then (
                    print_endline
                      "Please enter which piece you would like to \
                       promote this pawn to (Queen, Rook, Bishop, \
                       Knight).";
                    print_string ">";
                    let rec promote () =
                      let input = read_line () in
                      try
                        promotes next_state second_pos (newpiece input)
                      with Invalid_argument a -> promote ()
                    in
                    state (promote ()) true num_players)
                  else state next_state true num_players
              | _ ->
                  raise
                    (Malformed "Please enter a valid second position."))
        )
    | Castle d ->
        if d = "left" then state (castle s false) true num_players
        else if d = "right" then state (castle s true) true num_players
        else raise (Malformed "malformed in castle")
    | Rules ->
        rules "rules";
        state s false num_players
    | Quit ->
        print_endline "Thanks for playing!";
        exit 0
  with
  | Empty ->
      print_endline "Empty command. Please enter again.";
      state s false num_players
  | Malformed str ->
      print_endline "Wrong command structure. Please try again.";
      state s false num_players
  | Checkmate ->
      (print_endline "Checkmate";
       match s.turn with
       | White -> print_endline "White Wins"
       | Black -> print_endline "Black Wins");
      exit 0
  | Draw d ->
      print_endline ("Draw: " ^ d);
      exit 0
  | Invalid_argument a ->
      print_endline a;
      state s false num_players

let play_game num_players f =
  try state init_state true num_players
  with Sys_error f -> print_endline "Error in play_game"

(** [main ()] prompts for the game to play, then starts it. *)
let rec main (b : bool option) () =
  if b = Some false then
    print_endline
      "Welcome to chess! Type 'start' to play, 'quit' to quit the \
       game, or 'rules' to see the rules for playing."
  else if b = Some true then
    print_endline
      "Invalid input. Please type 'start' to play, 'quit' to exit the \
       game, or 'rules' to see the rules.";
  print_string ">";
  match read_line () with
  | exception End_of_file -> ()
  | str ->
      if str = "start" then (
        print_endline
          "Single-player or 2-player? Type '1' if single-player, '2' \
           if 2-player, or 'back' to return to startup. Or if you \
           would like to see two computers go crazy, type '0'";
        print_string ">";
        let rec start_game () =
          match read_line () with
          | exception End_of_file -> ()
          | s ->
              if s = "0" then play_game 0 str
              else if s = "1" then play_game 1 str
              else if s = "2" then play_game 2 str
              else if s = "back" then main (Some false) ()
              else
                print_endline
                  "Invalid input. Type '1' or '2' for a single-player \
                   or 2-player game, or 'back'";
              print_string ">";
              start_game ()
        in
        start_game ())
      else if str = "quit" then (
        print_endline
          "Leaving so soon? All good! Come back to play at any time!";
        ())
      else if str = "rules" then (
        rules "rules";
        main None ())
      else main (Some true) ()

(* Execute the game engine. *)
let () = main (Some false) ()
