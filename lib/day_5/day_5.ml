open ContainersLabels
open Angstrom

let test_input =
  [
    "    [D]    ";
    "[N] [C]    ";
    "[Z] [M] [P]";
    " 1   2   3 ";
    "";
    "move 1 from 2 to 1";
    "move 3 from 1 to 3";
    "move 2 from 2 to 1";
    "move 1 from 1 to 2";
  ]

type move = {
  amount : int;
  source : int;
  destination : int;
}

module P = struct
  let is_crate = function 'A' .. 'Z' -> true | _ -> false
  let is_bracket = function '[' | ']' -> true | _ -> false
  let is_space = function ' ' | '\t' -> true | _ -> false
end

let crate = take_while1 P.is_crate
let bracket = take_while1 P.is_bracket
let space = take_while P.is_space

let pooparser =
  parse_string ~consume:Consume.All
    (space *> bracket *> crate <* bracket <* space)

let t = match pooparser "[A]    " with Ok r -> r | Error _ -> failwith "ohno"

let parsers =
  Aoc_2022.Util.
    [
      parse "move %i from %i to %i" (fun amount source destination ->
          { amount; source; destination }
      );
    ]

let parse_instructions = Aoc_2022.Util.try_parse parsers

module A = struct
  let solve _l = 1

  let%test _ = solve test_input = 1
end

module B = struct
  let solve _l = 1

  let%test _ = solve test_input = 1
end

let run () =
  let input = Util.read_file "./lib/day_5/input" in

  let _puzzle_a = input |> A.solve |> Printf.printf "\nDay 5A: %i\n%!" in


  let _puzzle_b = input |> B.solve |> Printf.printf "Day 5B: %i\n%!" in

  ()
