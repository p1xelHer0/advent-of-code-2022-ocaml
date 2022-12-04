open ContainersLabels
open CCFun

let test_input =
  [
    "vJrwpWtwJgWrhcsFMMfFFhFp";
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL";
    "PmmdzqPrVvPwwTWBwg";
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn";
    "ttgJtRGJQctTZtZT";
    "CrZsJsPPZsGzwwsLwLmpwMDw";
  ]

let priority c =
  let c' = Char.code c in
  if c' > 96 then c' - 96 else c' - 38

module CSet = Set.Make (Char)

module A = struct
  let solve_aux input =
    let half = String.length input / 2 in
    let parsed_input = CCString.to_list input in
    let rec scan_comparment compartment items s1 s2 =
      match items with
      | [] ->
          CSet.inter s1 s2 |> CSet.elements |> List.map ~f:priority |> Util.sum
      | item :: tail ->
          if compartment >= half
          then scan_comparment (succ compartment) tail s1 (CSet.add item s2)
          else scan_comparment (succ compartment) tail (CSet.add item s1) s2
    in
    scan_comparment 0 parsed_input CSet.empty CSet.empty

  let solve input = input |> List.map ~f:solve_aux |> Util.sum

  let%test _ = solve test_input = 157
end

module B = struct
  let cset_of_string =
    String.fold_left ~f:(fun s item -> CSet.add item s) ~init:CSet.empty

  let solve_aux input =
    let rec scan_rucksacks commons items =
      match items with
      | r1 :: r2 :: r3 :: tail ->
          let s1 = cset_of_string r1 in
          let s2 = cset_of_string r2 in
          let s3 = cset_of_string r3 in
          let common = CSet.inter s1 s2 |> CSet.inter s3 in
          scan_rucksacks (common :: commons) tail
      | _ ->
          commons
          |> List.map ~f:(CSet.elements %> List.map ~f:priority %> Util.sum)
          |> Util.sum
    in
    scan_rucksacks [] input

  let solve input = input |> solve_aux

  let%test _ = solve test_input = 70
end

let run () =
  let input = Util.read_file "./lib/day_3/input" in

  let _puzzle_a = input |> A.solve |> Printf.printf "\nDay 3A: %i\n%!" in

  let _puzzle_b = input |> B.solve |> Printf.printf "Day 3B: %i\n%!" in

  ()
