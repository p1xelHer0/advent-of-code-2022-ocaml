open ContainersLabels
open CCFun

let print_matrix matrix =
  Array.iter
    ~f:(fun a ->
      Array.iter ~f:print_int a;
      print_newline ()
    )
    matrix;
  print_newline ()

module Tree = struct
  type t = {
    x : int;
    y : int;
    height : int;
  }

  let make ~x ~y height = { x; y; height }

  let compare t1 t2 =
    match compare t1.x t2.x with 0 -> compare t1.y t2.y | _ as x -> x

  let pp t =
    "(("
    ^ string_of_int t.x
    ^ ","
    ^ string_of_int t.y
    ^ "): "
    ^ string_of_int t.height
    ^ ")"
end

module TSet = CCSet.Make (Tree)

let to_matrix l =
  let parse_aux l =
    l
    |> String.to_list
    |> List.map ~f:(fun c -> int_of_char c - Char.code '0')
    |> Array.of_list
  in
  l
  |> String.trim
  |> String.split ~by:"\n"
  |> Array.of_list
  |> Array.map ~f:parse_aux

let test_input = {|
30373
25512
65332
33549
35390
|}

(* MUCH COPY PASTE HARD CODE EACH DIRECTION *)
let rec_down ~data:trees x result =
  let start_y = 0 in
  let first_tree = trees.{start_y, x} in
  let r = TSet.add (Tree.make ~x ~y:start_y first_tree) result in

  let rec aux y tree_line visible_trees =
    if y > pred @@ Bigarray.Array2.dim1 trees
    then visible_trees
    else
      let current_tree = trees.{y, x} in
      if List.for_all ~f:(( > ) current_tree) tree_line
      then
        let tree_line' = tree_line @ [ current_tree ] in
        let result = TSet.add (Tree.make ~x ~y current_tree) visible_trees in
        aux (succ y) tree_line' result
      else aux (succ y) tree_line visible_trees
  in
  aux (succ start_y) [ first_tree ] r

let rec_up ~data:trees x result =
  let start_y = pred @@ Bigarray.Array2.dim1 trees in
  let first_tree = trees.{start_y, x} in
  let r = TSet.add (Tree.make ~x ~y:start_y first_tree) result in

  let rec aux y tree_line visible_trees =
    if y = 0
    then visible_trees
    else
      let current_tree = trees.{y, x} in
      if List.for_all ~f:(( > ) current_tree) tree_line
      then
        let tree_line' = tree_line @ [ current_tree ] in
        let result = TSet.add (Tree.make ~x ~y current_tree) visible_trees in
        aux (pred y) tree_line' result
      else aux (pred y) tree_line visible_trees
  in
  aux (pred start_y) [ first_tree ] r

let rec_right ~data:trees y result =
  let start_x = 0 in
  let first_tree = trees.{y, start_x} in
  let r = TSet.add (Tree.make ~x:start_x ~y first_tree) result in

  let rec aux x tree_line visible_trees =
    if x > pred @@ Bigarray.Array2.dim2 trees
    then visible_trees
    else
      let current_tree = trees.{y, x} in
      if List.for_all ~f:(( > ) current_tree) tree_line
      then
        let tree_line' = tree_line @ [ current_tree ] in
        let result = TSet.add (Tree.make ~x ~y current_tree) visible_trees in
        aux (succ x) tree_line' result
      else aux (succ x) tree_line visible_trees
  in
  aux (succ start_x) [ first_tree ] r

let rec_left ~data:trees y result =
  let start_x = pred @@ Bigarray.Array2.dim2 trees in
  let first_tree = trees.{y, start_x} in
  let r = TSet.add (Tree.make ~x:start_x ~y first_tree) result in

  let rec aux x tree_line visible_trees =
    if x = 0
    then visible_trees
    else
      let current_tree = trees.{y, x} in
      if List.for_all ~f:(( > ) current_tree) tree_line
      then
        let tree_line' = tree_line @ [ current_tree ] in
        let result = TSet.add (Tree.make ~x ~y current_tree) visible_trees in
        aux (pred x) tree_line' result
      else aux (pred x) tree_line visible_trees
  in
  aux (pred start_x) [ first_tree ] r

let neighbour ~x ~y b2array =
  let w = Bigarray.Array2.dim2 b2array in
  let h = Bigarray.Array2.dim1 b2array in
  let bounds (x, y) = x >= 0 && y >= 0 && x < h && y < w in
  [ (x, succ y); (succ x, y); (x, pred y); (pred x, y) ]
  |> List.filter ~f:bounds

module A = struct
  let solve_aux l =
    let open CCList in
    let matrix = to_matrix l in
    let trees = Bigarray.(Array2.of_array Int c_layout matrix) in

    let y = 0 --^ Bigarray.Array2.dim1 trees in
    let x = 0 --^ Bigarray.Array2.dim2 trees in

    let left y init = y |> rec_left init ~data:trees in
    let right y init = y |> rec_right init ~data:trees in
    let up x init = x |> rec_up init ~data:trees in
    let down x init = x |> rec_down init ~data:trees in

    List.fold_left ~f:left ~init:TSet.empty y
    |> fun s ->
    List.fold_left ~f:right ~init:s y
    |> fun s ->
    List.fold_left ~f:up ~init:s x |> fun s -> List.fold_left ~f:down ~init:s x

  let solve l =
    let oo = solve_aux l in
    let _ = TSet.iter (fun t -> Printf.printf "%s\n" (Tree.pp t)) oo in
    oo |> TSet.cardinal

  let%test _ = solve test_input = 21
end

module B = struct
  let solve _l = 1

  let%test _ = solve test_input = 8
end

let run () =
  let input = Util.read_file_as_string "./lib/day_8/input" in

  let _puzzle_a = input |> A.solve |> Printf.printf "\nDay 8A: %i\n%!" in

  let _puzzle_b = input |> B.solve |> Printf.printf "Day 8B: %i\n%!" in

  ()
