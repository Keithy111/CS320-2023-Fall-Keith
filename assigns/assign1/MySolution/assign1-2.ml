(*
assign1-2: 10 points
Given two ordered strings cs1 and cs2, please use
string_make_fwork to implement string_merge(cs1, cs2)
which returns the order string obtained from merging
cs1 and cs2.

For instance, if cs1 = "135" and cs2 = "2468", then
string_merge(cs1)(cs2) equals "1234568"

For instance, if cs1 = "abcde" and cs2 = "1234", then
string_merge(cs1)(cs2) equals "1234abcde"
*)

(* ****** ****** *)


#use "./../MyOCaml.ml";;

let fwork_merge(cs1: string)(cs2: string)(work: char -> unit): unit =
  let len1 = string_length cs1 in
  let len2 = string_length cs2 in
  let i = ref 0 in
  let j = ref 0 in
  while !i < len1 && !j < len2 do
    let char1 = string_get_at cs1 !i in
    let char2 = string_get_at cs2 !j in
    if char1 <= char2 then begin
      work char1;
      incr i;
    end else begin
      work char2;
      incr j;
    end;
  done;
  
  (* Append any remaining characters from cs1 *)
  while !i < len1 do
    let char1 = string_get_at cs1 !i in
    work char1;
    incr i;
  done;

  (* Append any remaining characters from cs2 *)
  while !j < len2 do
    let char2 = string_get_at cs2 !j in
    work char2;
    incr j;
  done;
;;

let string_merge(cs1: string) (cs2: string): string =
  let merged_chars = ref [] in
  let append_char(char: char) = merged_chars := char :: !merged_chars in
  fwork_merge cs1 cs2 append_char;
  let merged_string = string_make_fwork (fun work -> List.iter work !merged_chars) in
  merged_string;
;;