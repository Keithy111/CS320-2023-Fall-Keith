(*
//
Assign4-1:
//
HX-2023-10-05: 10 points
//
The following is a well-known series:
ln 2 = 1 - 1/2 + 1/3 - 1/4 + ...
Please implement a stream consisting of all the
partial sums of this series.
The 1st item in the stream equals 1
The 2nd item in the stream equals 1 - 1/2
The 3rd item in the stream equals 1 - 1/2 + 1/3
The 4th item in the stream equals 1 - 1/2 + 1/3 - 1/4
And so on, and so forth
//
let the_ln2_stream: float stream = fun() -> ...
//
*)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let the_ln2_stream: float stream = 
  let rec streamer n sign denominator sum = fun () ->
    let summ = sign /. denominator in 
    let new_sum = if n = 1 then 1.0 else sum +. summ in 
    StrCons (new_sum, streamer (n+1)(-.sign)(denominator +.1.0) new_sum)
  in
  streamer 1 (1.0)(1.0) (1.0)
