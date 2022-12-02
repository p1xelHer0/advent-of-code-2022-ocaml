open ContainersLabels

let test_input = "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"

let solve' l =
  l |> CCStringLabels.lines |> List.map ~f:int_of_string |> Util.sum

module A = struct
  let solve l =
    List.(
      l
      |> CCStringLabels.split ~by:"\n\n"
      |> map ~f:solve'
      |> sort ~cmp:CCInt.compare
      |> rev
      |> hd
    )

  let%test _ = solve test_input = 24000
end

module B = struct
  let solve l =
    List.(
      l
      |> CCStringLabels.split ~by:"\n\n"
      |> map ~f:solve'
      |> sort ~cmp:CCInt.compare
      |> rev
      |> take 3
      |> Util.sum
    )

  let%test _ = solve test_input = 45000
end

let run () =
  let input = "./lib/day_1/input" in

  let parsed_input = input |> Util.read_file_as_string in

  let _puzzle_a = parsed_input |> A.solve |> Printf.printf "\nDay 1A: %i\n%!" in

  let _puzzle_b = parsed_input |> B.solve |> Printf.printf "Day 1B: %i\n%!" in

  ()
