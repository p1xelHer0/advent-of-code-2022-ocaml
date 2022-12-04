open ContainersLabels

let test_input =
  [ "2-4,6-8"; "2-3,4-5"; "5-7,7-9"; "2-8,3-7"; "6-6,4-6"; "2-6,4-8" ]

let parsers =
  Aoc_2022.Util.[ parse "%i-%i,%i-%i" (fun i j k l -> ((i, j), (k, l))) ]

let parse_instructions = Aoc_2022.Util.try_parse parsers
let included (i, j) (k, l) = k <= i && j <= l
let disjoint (i, j) (k, l) = j < k || l < i

module A = struct
  let solve input =
    let section_assignment_pairs = List.map ~f:parse_instructions input in
    let range_included (i, j) = included i j || included j i in
    List.filter ~f:range_included section_assignment_pairs |> List.length

  let%test _ = solve test_input = 2
end

module B = struct
  let solve input =
    let section_assignment_pairs = List.map ~f:parse_instructions input in
    let range_disjoint (i, j) = not (disjoint i j || disjoint j i) in
    List.filter ~f:range_disjoint section_assignment_pairs |> List.length

  let%test _ = solve test_input = 4
end

let run () =
  let input = Util.read_file "./lib/day_4/input" in

  let _puzzle_a = input |> A.solve |> Printf.printf "\nDay 4A: %i\n%!" in

  let _puzzle_b = input |> B.solve |> Printf.printf "Day 4B: %i\n%!" in

  ()
