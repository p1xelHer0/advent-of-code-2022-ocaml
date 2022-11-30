open ContainersLabels

module A = struct
  let solve _l = 1

  let%test _ = solve [] = 1
end

module B = struct
  let solve _l = 1

  let%test _ = solve [] = 1
end

let run () =
  let input = "./lib/day_10/input" in

  let parsed_input = input |> Util.read_file in

  let _puzzle_a = parsed_input |> A.solve |> Printf.printf "\nDay 10A: %i\n%!" in

  let _puzzle_b = parsed_input |> B.solve |> Printf.printf "Day 10B: %i\n%!" in

  ()
