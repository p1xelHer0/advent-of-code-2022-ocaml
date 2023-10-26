open ContainersLabels

let test_input =
  [
    "$ cd /";
    "$ ls";
    "dir a";
    "14848514 b.txt";
    "8504156 c.dat";
    "dir d";
    "$ cd a";
    "$ ls";
    "dir e";
    "29116 f";
    "2557 g";
    "62596 h.lst";
    "$ cd e";
    "$ ls";
    "584 i";
    "$ cd ..";
    "$ cd ..";
    "$ cd d";
    "$ ls";
    "4060174 j";
    "8033020 d.log";
    "5626152 d.ext";
    "7214296 k";
  ]

type instruction =
  | List_Files
  | Change_Directory_Root
  | Change_Directory_Up
  | Change_Directory_To of string
  | Directory of string
  | File of int * string

let parsers =
  Aoc_2022.Util.
    [
      parse "$ ls" (fun () -> List_Files);
      parse "$ cd /" (fun () -> Change_Directory_Root);
      parse "$ cd .." (fun () -> Change_Directory_Up);
      (* parse "$ cd %s" (fun dir -> Change_Directory_To dir); *)
      (* parse "dir %s" (fun dir -> Directory dir); *)
      (* parse "%i %s " (fun size name -> File (size, name)); *)
    ]

let parse_instructions = Aoc_2022.Util.try_parse parsers

module A = struct
  let solve l =
    let _parsed_input = List.map ~f:parse_instructions l in
    1

  (* let%test _ = solve test_input = 1 *)
end

module B = struct
  let solve _l = 1

  (* let%test _ = solve test_input = 1 *)
end

let run () =
  let parsed_input = Util.read_file "./lib/day_7/input" in

  let _puzzle_a = parsed_input |> A.solve |> Printf.printf "\nDay 7A: %i\n%!" in

  let _puzzle_b = parsed_input |> B.solve |> Printf.printf "Day 7B: %i\n%!" in

  ()
