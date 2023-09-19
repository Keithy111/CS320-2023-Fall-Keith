(* ************************************************ *)

(*

 Question 8: 20 points
 Please give a NON-RECURSIVE implementation of sort5
 that takes 5 integers and returns a tuple that consists
 exactly of the 5 given integers ordered increasingly

 For instance, sort5(1, 2, 1, 2, 1) = (1, 1, 1, 2, 2)
 For instance, sort5(1, 3, 4, 5, 2) = (1, 2, 3, 4, 5)
 For instance, sort5(1, 3, 5, 4, 2) = (1, 2, 3, 4, 5)

 You can implement your own helper functions as long as
 you do not make use of recursion.

*)

let sort5 (a, b, c, d, e) =
  let min_val = List.fold_left min a [b; c; d; e] in
  let max_val = List.fold_left max a [b; c; d; e] in
  let middle_val =
    let sum = a + b + c + d + e in
    sum - min_val - max_val
  in
  let sorted_list = [min_val; middle_val; max_val] in
  let sorted_tuple = (List.nth sorted_list 0, List.nth sorted_list 1, List.nth sorted_list 2, List.nth sorted_list 3, List.nth sorted_list 4) in
  sorted_tuple