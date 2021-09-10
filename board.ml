open Pieces

type position = int * int

type tile = position * piece option

type t = tile list

let wpawn = set_piece White Pawn false

let wknight = set_piece White Knight false

let wbishop = set_piece White Bishop false

let wrook = set_piece White Rook false

let wqueen = set_piece White Queen false

let wking = set_piece White King false

let bpawn = set_piece Black Pawn false

let bknight = set_piece Black Knight false

let bbishop = set_piece Black Bishop false

let brook = set_piece Black Rook false

let bqueen = set_piece Black Queen false

let bking = set_piece Black King false

let white_pieces : t =
  [
    ((1, 1), Some wrook);
    ((2, 1), Some wknight);
    ((3, 1), Some wbishop);
    ((4, 1), Some wqueen);
    ((5, 1), Some wking);
    ((6, 1), Some wbishop);
    ((7, 1), Some wknight);
    ((8, 1), Some wrook);
    ((1, 2), Some wpawn);
    ((2, 2), Some wpawn);
    ((3, 2), Some wpawn);
    ((4, 2), Some wpawn);
    ((5, 2), Some wpawn);
    ((6, 2), Some wpawn);
    ((7, 2), Some wpawn);
    ((8, 2), Some wpawn);
  ]

let black_pieces : t =
  [
    ((1, 8), Some brook);
    ((2, 8), Some bknight);
    ((3, 8), Some bbishop);
    ((4, 8), Some bqueen);
    ((5, 8), Some bking);
    ((6, 8), Some bbishop);
    ((7, 8), Some bknight);
    ((8, 8), Some brook);
    ((1, 7), Some bpawn);
    ((2, 7), Some bpawn);
    ((3, 7), Some bpawn);
    ((4, 7), Some bpawn);
    ((5, 7), Some bpawn);
    ((6, 7), Some bpawn);
    ((7, 7), Some bpawn);
    ((8, 7), Some bpawn);
  ]

let blank_tiles : t =
  let blank_list = [] in
  let rec helper i j lst =
    match j with
    | 7 -> lst
    | int -> (
        match i with
        | 9 -> helper 1 (j + 1) lst
        | int -> helper (i + 1) j (((i, j), None) :: lst))
  in
  helper 1 3 blank_list

let new_board : t = white_pieces @ black_pieces @ blank_tiles

let blank_board : t =
  let blank_list = [] in
  let rec helper i j lst =
    match j with
    | 9 -> lst
    | int -> (
        match i with
        | 9 -> helper 1 (j + 1) lst
        | int -> helper (i + 1) j (((i, j), None) :: lst))
  in
  helper 1 1 blank_list

let print_board t =
  for j = 1 to 8 do
    print_endline
      "  \
       —————————————————————————";
    print_int (9 - j);
    print_string " ";
    for i = 1 to 8 do
      print_string "|";
      match List.assoc (i, 9 - j) t with
      | Some p -> print_string (piece_to_string p ^ " ")
      | None -> print_string "  "
    done;
    print_endline "|"
  done;
  print_endline
    "  \
     —————————————————————————";
  print_endline "   a  b  c  d  e  f  g  h"
