(*
Assign3-1:
HX-2023-09-26: 10 points
A matrix can be represented as a list of lists.
For instance, [[1;2;3];[4;5;6];[7;8;9]] represents
the following 3x3 square matrix:
1 2 3
4 5 6
7 8 9
Please implement matrix_transpose that returns
the transpose of a given matrix:
let rec
matrix_transpose(xss: 'a list list): 'a list list
For instance, the transpose of the above matrix
is given as follows:
1 4 7
2 5 8
3 6 9
You are allowed to define recursive functions to
solve this problem.
*)

#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let rec matrix_transpose (xss: 'a list list): 'a list list =
  match xss with
  | [] -> []  (* Empty matrix, return empty matrix *)
  | [] :: _ -> []  (* Empty row, skip it *)
  | _ ->
    let first_column = List.map List.hd xss in
    let rest_columns = matrix_transpose (List.map List.tl xss) in
    first_column :: rest_columns
;;