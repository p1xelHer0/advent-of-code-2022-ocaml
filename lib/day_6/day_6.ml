open ContainersLabels

let test_input = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"

let solve' l distinct_markers =
  let rec aux input string i =
    match input with
    | [] -> failwith "end of file"
    | c :: tl ->
        if List.length string >= distinct_markers
        then i
        else if List.mem c string
        then
          let string' =
            match
              string |> Array.of_list |> Array.find_idx ~f:(Char.equal c)
            with
            | Some (i, _) -> List.drop (succ i) string @ [ c ]
            | None -> string
          in
          aux tl string' (succ i)
        else aux tl (string @ [ c ]) (succ i)
  in
  let input = String.to_list l in
  aux input [] 0

module A = struct
  let solve l = solve' l 4

  let%test _ = solve test_input = 11
end

module B = struct
  let solve l = solve' l 14

  let%test _ = solve test_input = 26
end

let run () =
  let input = Util.read_file_as_string "./lib/day_6/input" in

  let _puzzle_a = input |> A.solve |> Printf.printf "\nDay 6A: %i\n%!" in

  let _puzzle_b = input |> B.solve |> Printf.printf "Day 6B: %i\n%!" in
  ()
