(*
assign1-3: 10 points
A 3-letter sequence abc is 132-like
if a < c < b holds. For instance, 123 is
not 132-like; but 596 is 132-like.

A string is 132-avoid if there is no subsequence
abc in this string that is 132-like. Note that a
subsequence of a string may contain NON-CONSEQUTIVE
letters of the string.

For instance, 123456789 is 132-avoid;
For instance, 987654321 is 132-avoid;
For instance, 123465789 is not 132-avoid since the
subsequence 165 is 132-like.

Please implement a function string_avoid_132 that
checks if a given string is 132-avoid; the function
returns true if and only if the given string is 132-
avoid.

fun string_avoid_132(cs: string): bool
*)

(* ****** ****** *)

#use "./../assign0.ml";;

let string_avoid_132(cs: string): bool =
  let len = string_length cs in
  if len < 3 then true  (* Strings with less than 3 characters are always 132-avoid. *)
  else
    let rec check_avoid i a b =
      if i >= len then true  (* Reached the end of the string without finding a 132-like sequence. *)
      else
        let c = string_get_at cs i in
        if ord(a) < ord(c) && ord(c) < ord(b) then false  (* Found a 132-like sequence. *)
        else if ord(a) > ord(c) then check_avoid (i + 1) a b  (* Update 'a' if necessary. *)
        else check_avoid (i + 1) a c  (* Update 'b' if necessary. *)
    in
    check_avoid 1 (string_get_at cs 0) (string_get_at cs 1)
;;

