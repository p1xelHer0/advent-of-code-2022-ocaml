open ContainersLabels

let test_input =
  {|
[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]
|}

module Parser = struct
  open Angstrom

  let bracket_open = char '['
  let bracket_close = char ']'

  let integer =
    take_while (function '0' .. '9' -> true | _ -> false) >>| int_of_string
end

module A = struct
  let solve _l = 1

  let%test _ = solve [] = 1
end

module B = struct
  let solve _l = 1

  let%test _ = solve [] = 1
end

let run () =
  let input = Util.read_file_as_string "./lib/day_13/input" |> String.trim in

  let _puzzle_a = input |> A.solve |> Printf.printf "\nDay 13A: %i\n%!" in

  let _puzzle_b = input |> B.solve |> Printf.printf "Day 13B: %i\n%!" in

  ()
