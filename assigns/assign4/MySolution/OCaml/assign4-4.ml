(*
//
Assign4-4:
//
HX-2023-10-05: 20 points
//
Please enumerate all the permuations of a
given list. The enumeration is required to be
in order. For instance, say xs = [1;2;3], then
the enumeration should be of the following order
[1;2;3], [1;3;2], [2;1;3], [2;3;1], [3;1;2], [3;2;1].
//
let list_permute(xs: 'a list): 'a list stream
*)

(* ****** ****** *)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let rec insert_everywhere x lst =
  let rec insert_at x lst n acc =
    match lst with
    | [] -> List.rev_append acc [x]
    | hd :: tl when n = 0 -> List.rev_append acc (x :: hd :: tl)
    | hd :: tl -> insert_at x tl (n - 1) (hd :: acc)
  in
  let len = List.length lst in
  let rec insert_at_all x lst n acc =
    if n = len then
      List.rev_append acc [x :: lst]
    else
      insert_at_all x lst (n + 1) (insert_at x lst n [] :: acc)
  in
  insert_at_all x lst 0 []

let rec permutations xs =
  match xs with
  | [] -> [[]]
  | x :: xs' ->
    let perms = permutations xs' in
    List.flatten (List.map (insert_everywhere x) perms)

let list_permute xs = permutations xs
