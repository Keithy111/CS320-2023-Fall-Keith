(*
assign1-5: 20 points

A sequence of chars is ascending if any char in
the sequence is less than or equal to the following
one (when the following one does exist).
Given a string cs, please implement a function
that find the longest ascending subsequence of [cs].
If there are more than one such sequences, the left
most one should be returned.

fun string_longest_ascend(xs: string): string

For instance, given "1324561111", the function
string_longest_ascend returns "13456"

For instance, given "1234561111", the function
string_longest_ascend returns "123456"

For instance, given "1234511111", the function
string_longest_ascend returns "111111".
*)

(* ****** ****** *)

#use "./../MyOCaml.ml";;

let str(c0) = String.make 1 c0;;

let string_longest_ascend(cs: string): string =
  let n = string_length cs in
  if n = 0 then "" (* Handle the case of an empty string *)
  else begin
    let longest_seq = ref (str cs.[0]) in
    let cur_seq = ref (str cs.[0]) in
    let i = ref 0 in
    
    while !i < n - 1 do
      let cur_char = cs.[!i] in
      let next_char = cs.[!i + 1] in
      
      if cur_char <= next_char then
        (* Continue the ascending subsequence *)
        cur_seq := !cur_seq ^ (str next_char)
      else
        (* Start a new ascending subsequence *)
        cur_seq := str next_char;
      
      (* Update the longest subsequence if needed *)
      if string_length !cur_seq > string_length !longest_seq then
        longest_seq := !cur_seq;
      
      i := !i + 1;
    done;
    
    !longest_seq
  end
;;
