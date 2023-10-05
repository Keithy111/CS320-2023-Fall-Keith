(* ************************************************ *)

(*
Q2-6: 10 points

The function list_reverse return the reverse of a given list.
Please give an implementation of list_reverse based on list_foldright
(not list_foldleft).
*)

(* ************************************************ *)

let list_reverse(xs: 'a list): 'a list = ....
(xs: 'a mylist)
(r0: 'r0)(fopr: 'r0 -> 'a -> 'r0): 'r0 =
match xs with
| MyNil -> r0
| MyCons(x1, xs) -> mylist_foldleft(xs)(fopr(r0)(x1))(fopr)
;;