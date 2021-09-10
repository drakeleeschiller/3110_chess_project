open Board

type object_phrase = string list

type command =
  | Go of object_phrase
  | Castle of string
  | Rules
  | Quit

exception Empty

exception Malformed of string

let parse str =
  let str_list =
    List.filter (( <> ) "") (String.split_on_char ' ' str)
  in
  match str_list with
  | [] -> raise Empty
  | [ h; t ] ->
      if h = "castle" then Castle t
      else raise (Malformed "malformed in parse")
  | h :: t ->
      if h = "rules" && t = [] then Rules
      else if h = "go" && t != [] then Go t
      else if h = "quit" && t = [] then Quit
      else
        raise
          (Malformed
             "Please use the command [go (position of piece to move) \n\
             \      (position to move to)] or [quit] to stop playing.")

(* [str_to_chars] takes a string s and turns it into a list of the
   characters of s *)
let str_to_chars s = List.init (String.length s) (String.get s)

(* [str_to_pos] takes in a string and turns it into a board position.
   Raises: [Empty] if [str] is the empty string.

   Raises: [Malformed] if the command is malformed. A command is
   malformed if str is not in the correct format (e.g. single character,
   more than 2 characters, invalid position) *)
let str_to_pos str : Board.position =
  let char_list = str_to_chars str in
  if List.length char_list != 2 then
    raise
      (Malformed
         "Please use a character and a number to represent a position \
          e.g. a3, h6, etc.")
  else
    match char_list with
    | [] -> raise (Malformed "Please enter a position")
    | h :: t -> (
        if t = [] then raise (Malformed "malformed in str_to_pos 1")
        else
          let first_int = int_of_char h - 96 in
          match t with
          | a :: b ->
              let second_int = int_of_char a - 48 in
              if
                first_int > 0 && first_int < 9 && second_int > 0
                && second_int < 9
              then (first_int, second_int)
              else raise (Malformed "malformed in str_to_pos 2")
          | _ -> raise (Malformed "malformed in str_to_pos 3"))
