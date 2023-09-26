(* ****** ****** *)

(*
//
Assign2-3: 10 points
//
Please implement foldleft_to_iforeach that turns a
foldleft-loop into a iforeach-loop:
let
foldleft_to_iforeach
(foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach = ...
*)

(* ****** ****** *)

#use "./../../assign2.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let foldleft_to_iforeach (foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach =
  fun xs f ->
    let rec loop i r0 xs =
      match xs with
      | [] -> ()
      | x :: xs' ->
        f i x;
        let r0' = f r0 x in
        loop (i + 1) r0' xs'
    in
    loop 0 foldleft xs;
    ()



  

