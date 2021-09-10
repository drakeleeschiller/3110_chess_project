val random_move : State.t -> State.t

val optimal_move_1_step : State.t -> State.t

val optimal_move : State.t -> State.t

type 'a tree = T of 'a * 'a tree list

val moves_tree : State.t -> int -> (State.t * int) tree
