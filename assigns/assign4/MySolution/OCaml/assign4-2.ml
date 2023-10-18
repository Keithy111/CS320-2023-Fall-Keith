(*
//
Assign4-2:
//
HX-2023-10-05: 10 points
//
Please enumerate all the pairs of natural
numbers. Given pairs (i1, j1) and (i2, j2),
(i1, j1) should be enumerated ahead of (i2, j2)
if i1+j1 < i2+j2.
//
let theNatPairs: (int*int) stream = fun () -> ...
//
*)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type ('a, 'b) pair = 'a * 'b

let rec generate_pairs i1 j1 i2 j2 =
  if i1 + j1 <= i2 + j2 then
    StrCons ((i1, j1), fun () -> generate_pairs (i1 + 1) j1 i2 j2)
  else
    generate_pairs 0 (j1 + 1) 0 (j1 + 1)

let theNatPairs : (int * int) stream =
  generate_pairs 0 0 0 0


