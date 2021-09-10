open State
open Pieces

(** random move generator*)
let random_move st =
  Random.self_init ();
  let moves_list = all_moves st in
  List.nth moves_list (Random.int (List.length moves_list))

let piece_value = function
  | Pawn -> 10
  | Knight | Bishop -> 30
  | Rook -> 50
  | Queen -> 90
  | King -> 900

let team_pieces team st =
  team_tiles team st
  |> List.map (fun t -> t |> snd |> Option.get |> get_piece_type)

let material_eval st =
  let rec material acc = function
    | [] -> acc
    | h :: t -> material (acc + piece_value h) t
  in
  material 0 (team_pieces White st) - material 0 (team_pieces Black st)

let eval st =
  if is_checkmate st then
    match st.turn with White -> -99999 | Black -> 99999
  else if is_stalemate st || insufficient_material st then 0
  else material_eval st

let sort_all_moves_by_eval st =
  all_moves st
  |> List.map (fun s -> (s, material_eval s))
  |> List.sort (fun s1 s2 -> compare (snd s1) (snd s2))

type 'a tree = T of 'a * 'a tree list

let rec moves_tree st depth =
  if depth = 0 then T ((st, eval st), [])
  else
    T
      ( (st, eval st),
        List.map (fun s -> moves_tree s (depth - 1)) (all_moves st) )

let is_leaf tree = match tree with T (_, []) -> true | _ -> false

let children tree =
  match tree with T (_, []) -> failwith "empty tree" | T (_, l) -> l

(** greedy 1-depth move optimization*)
let optimal_move_1_step st =
  let sorted_moves = sort_all_moves_by_eval st in
  match st.turn with
  | White -> sorted_moves |> List.rev |> List.hd |> fst
  | Black -> sorted_moves |> List.hd |> fst

let depth = 2

let get_state = function T ((s, e), _) -> s

let max i1 i2 = if i1 > i2 then i1 else i2

let min i1 i2 = if i1 > i2 then i2 else i1

let rand_cond () = Random.float 1.0 <= 0.2

exception Prune

(** uses the minimax algorithm to find the optimal move at a given depth*)
let optimal_move st =
  Random.self_init ();
  let rec minimax s dep maxing alpha beta =
    if dep = 0 then (s, eval s)
    else
      let next_moves =
        all_moves s |> List.sort (fun a b -> 5 - Random.int 11)
      in
      if next_moves = [] then (s, eval s)
      else if maxing then (
        let best_value = ref (-999999) in
        let best_move = ref None in
        let ralpha = ref alpha in
        (let rec loop lst =
           match lst with
           | [] -> ()
           | h :: t ->
               let value =
                 snd (minimax h (dep - 1) false !ralpha beta)
               in
               if value > !best_value then (
                 (* print_string " Max: "; print_int value; print_string
                    " Depth: "; print_int (depth - dep); *)
                 best_value := value;
                 best_move := Some h)
               else ();
               ralpha := max !ralpha value;
               if beta <= !ralpha then
                 ( (* print_endline ("Max Prune, Alpha = " ^
                      string_of_int !ralpha ^ ", Beta = " ^
                      string_of_int beta); () *) )
               else loop t
         in
         loop next_moves);
        (Option.get !best_move, !best_value))
      else
        let best_value = ref 999999 in
        let best_move = ref None in
        let rbeta = ref beta in
        (let rec loop lst =
           match lst with
           | [] -> ()
           | h :: t ->
               let value =
                 snd (minimax h (dep - 1) true alpha !rbeta)
               in
               if value < !best_value then (
                 (* print_string " Min: "; print_int value; print_string
                    " Depth: "; print_int (depth - dep); *)
                 best_value := value;
                 best_move := Some h)
               else ();
               rbeta := min !rbeta value;
               if !rbeta <= alpha then
                 ( (* print_endline ("Min Prune, Alpha = " ^
                      string_of_int alpha ^ ", Beta = " ^ string_of_int
                      !rbeta); () *) )
               else loop t
         in
         loop next_moves);
        (Option.get !best_move, !best_value)
  in
  let best_move =
    match st.turn with
    | White -> minimax st depth true (-999999) 999999
    | Black -> minimax st depth false (-999999) 999999
  in
  fst best_move
