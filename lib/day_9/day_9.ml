open ContainersLabels

let test_input = {|
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
|}

type direction =
  | Up
  | Right
  | Left
  | Down

let direction_of_string = function
  | 'U' -> Up
  | 'R' -> Right
  | 'L' -> Left
  | 'D' -> Down
  | _ -> failwith "ohno"

module Coordinate = struct
  type t = {
    x : int;
    y : int;
  }

  let make ~x ~y = { x; y }

  let compare t1 t2 =
    match compare t1.x t2.x with 0 -> compare t1.y t2.y | _ as x -> x

  let distance ~from:t1 ~_to:t2 = abs (t2.x - t1.x) + abs (t2.y - t1.y)

  let move t ~direction ~distance =
    match direction with
    | Up -> { t with y = t.y + distance }
    | Right -> { t with x = t.x + distance }
    | Left -> { t with x = t.x - distance }
    | Down -> { t with y = t.y - distance }

  let pp t = "(" ^ string_of_int t.x ^ "," ^ string_of_int t.y ^ ")"
end

module CSet = Set.Make (Coordinate)

let parsers =
  Aoc_2022.Util.
    [
      parse "%c %i" (fun direction steps ->
          (direction_of_string direction, steps)
      );
    ]

let parse_instructions = Aoc_2022.Util.try_parse parsers

module A = struct
  let solve input =
    let start = Coordinate.make ~x:0 ~y:0 in
    let instructions =
      input |> String.split ~by:"\n" |> List.map ~f:parse_instructions
    in
    snd
      (List.fold_left_map
         ~f:(fun c (direction, distance) ->
           (Coordinate.move ~direction ~distance c, c)
         )
         ~init:start instructions
      )

  (* let%test _ = solve test_input = 1 *)
end

module B = struct
  let solve _l = 1

  let%test _ = solve [] = 1
end

let run () =
  let _input = Util.read_file_as_string "./lib/day_9/input" |> String.trim in

  (* let _puzzle_a = input |> A.solve |> Printf.printf "\nDay 9A: %i\n%!" in *)

  (* let _puzzle_b = input |> B.solve |> Printf.printf "Day 9B: %i\n%!" in *)
  ()
