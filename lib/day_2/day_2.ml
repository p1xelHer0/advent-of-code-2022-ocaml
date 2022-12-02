open ContainersLabels

type player =
  | X
  | Y
  | Z

type opponent =
  | A
  | B
  | C

type game_state =
  | Win
  | Lose
  | Draw

type shape =
  | Rock
  | Paper
  | Scissor

type outcome = {
  shape : shape;
  game_state : game_state;
}

type game_1 = {
  opponent : shape;
  player : shape;
}

type game_2 = {
  opponent : shape;
  expected_game_state : game_state;
}

let opponent_of_char c =
  match c with
  | 'A' -> A
  | 'B' -> B
  | 'C' -> C
  | _ -> failwith "invalid character in input"

let player_of_char c =
  match c with
  | 'X' -> X
  | 'Y' -> Y
  | 'Z' -> Z
  | _ -> failwith "invalid character in input"

let shape_of_opponent c = match c with A -> Rock | B -> Paper | C -> Scissor
let shape_of_player c = match c with X -> Rock | Y -> Paper | Z -> Scissor
let game_state_of_player p = match p with X -> Lose | Y -> Draw | Z -> Win
let score_of_shape s = match s with Rock -> 1 | Paper -> 2 | Scissor -> 3
let score_of_gamestate o = match o with Win -> 6 | Lose -> 0 | Draw -> 3

let score_of_outcome ~game_state ~shape =
  score_of_gamestate game_state + score_of_shape shape

let play_1 ~player ~opponent =
  match (player, opponent) with
  | Rock, Rock -> { shape = player; game_state = Draw }
  | Rock, Paper -> { shape = player; game_state = Lose }
  | Rock, Scissor -> { shape = player; game_state = Win }
  | Paper, Paper -> { shape = player; game_state = Draw }
  | Paper, Rock -> { shape = player; game_state = Win }
  | Paper, Scissor -> { shape = player; game_state = Lose }
  | Scissor, Scissor -> { shape = player; game_state = Draw }
  | Scissor, Rock -> { shape = player; game_state = Lose }
  | Scissor, Paper -> { shape = player; game_state = Win }

let play_2 ~expected_game_state ~opponent =
  match (expected_game_state, opponent) with
  | Win, Rock -> play_1 ~player:Paper ~opponent
  | Win, Paper -> play_1 ~player:Scissor ~opponent
  | Win, Scissor -> play_1 ~player:Rock ~opponent
  | Lose, Rock -> play_1 ~player:Scissor ~opponent
  | Lose, Paper -> play_1 ~player:Rock ~opponent
  | Lose, Scissor -> play_1 ~player:Paper ~opponent
  | Draw, Rock -> play_1 ~player:opponent ~opponent
  | Draw, Paper -> play_1 ~player:opponent ~opponent
  | Draw, Scissor -> play_1 ~player:opponent ~opponent

module A = struct
  let parsers =
    let parse_opponent c = opponent_of_char c |> shape_of_opponent in
    let parse_player c = player_of_char c |> shape_of_player in
    Aoc_2022.Util.
      [
        parse "%c %c" (fun c1 c2 ->
            { opponent = parse_opponent c1; player = parse_player c2 }
        );
      ]

  let parse_instructions = Aoc_2022.Util.try_parse parsers

  let solve' game =
    play_1 ~opponent:game.opponent ~player:game.player
    |> fun outcome ->
    score_of_outcome ~game_state:outcome.game_state ~shape:outcome.shape

  let solve l =
    let instructions = l |> List.map ~f:parse_instructions in
    instructions |> List.map ~f:solve' |> List.fold_left ~f:( + ) ~init:0

  let%test _ = solve [] = 1
end

module B = struct
  let parsers =
    let parse_opponent c = opponent_of_char c |> shape_of_opponent in
    let parse_expected_game_state c =
      player_of_char c |> game_state_of_player
    in
    Aoc_2022.Util.
      [
        parse "%c %c" (fun c1 c2 ->
            {
              opponent = parse_opponent c1;
              expected_game_state = parse_expected_game_state c2;
            }
        );
      ]

  let parse_instructions = Aoc_2022.Util.try_parse parsers

  let solve' game =
    play_2 ~opponent:game.opponent ~expected_game_state:game.expected_game_state
    |> fun outcome ->
    score_of_outcome ~game_state:outcome.game_state ~shape:outcome.shape

  let solve l =
    let instructions = l |> List.map ~f:parse_instructions in
    instructions |> List.map ~f:solve' |> List.fold_left ~f:( + ) ~init:0

  let%test _ = solve [] = 1
end

let run () =
  let input = "./lib/day_2/input" in

  let parsed_input = input |> Util.read_file in

  let _puzzle_a = parsed_input |> A.solve |> Printf.printf "\nDay 2A: %i\n%!" in

  let _puzzle_b = parsed_input |> B.solve |> Printf.printf "Day 2B: %i\n%!" in

  ()
