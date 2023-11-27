#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-1.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)


type const =
  | Int of int
  | Bool of bool
  | Unit

type com =
  | Push of const
  | Pop
  | Trace
  | Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | Not
  | Lt
  | Gt

type prog = com list

let rec interp_const (c : const) : string =
   match c with
   | Int n -> string_of_int n
   | Bool b -> if b then "True" else "False"
   | Unit -> "Unit"
 
let rec interp_com (com : com) (stack : int list) : (string list * int list) option =
   match com with
   | Push c -> (interp_const c :: [], c :: stack)
   | Pop ->
     (match stack with
      | [] -> None
      | _ :: rest -> ("Pop" :: [], rest))
   | Trace -> ("Trace" :: [], stack)
   | Add ->
     (match stack with
      | x :: y :: rest -> (string_of_int (x + y) :: [], (x + y) :: rest)
      | _ -> None)
   | Sub ->
     (match stack with
      | x :: y :: rest -> (string_of_int (y - x) :: [], (y - x) :: rest)
      | _ -> None)
   | Mul ->
     (match stack with
      | x :: y :: rest -> (string_of_int (x * y) :: [], (x * y) :: rest)
      | _ -> None)
   | Div ->
     (match stack with
      | x :: y :: rest ->
        if x = 0 then None
        else (string_of_int (y / x) :: [], (y / x) :: rest)
      | _ -> None)
   | And ->
     (match stack with
      | x :: y :: rest -> (interp_const (Bool (x <> 0 && y <> 0)) :: [], (if x <> 0 && y <> 0 then 1 else 0) :: rest)
      | _ -> None)
   | Or ->
     (match stack with
      | x :: y :: rest -> (interp_const (Bool (x <> 0 || y <> 0)) :: [], (if x <> 0 || y <> 0 then 1 else 0) :: rest)
      | _ -> None)
   | Not ->
     (match stack with
      | x :: rest -> (interp_const (Bool (x = 0)) :: [], (if x = 0 then 1 else 0) :: rest)
      | _ -> None)
   | Lt ->
     (match stack with
      | x :: y :: rest -> (interp_const (Bool (y < x)) :: [], if y < x then 1 else 0 :: rest)
      | _ -> None)
   | Gt ->
      (match stack with
      | x :: y :: rest -> (interp_const (Bool (y > x)) :: [], if y > x then 1 else 0 :: rest)
      | _ -> None)
       
         
let rec interp_coms (prog : prog) (stack : int list) : (string list * int list) option =
   match prog with
   | [] -> Some ([], stack)
   | com :: rest ->
      match interp_com com stack with
      | Some (output, new_stack) ->
         (match interp_coms rest new_stack with
         | Some (rest_output, final_stack) -> Some (output @ rest_output, final_stack)
         | None -> None)
      | None -> None
    

let interp (s : string) : string list option =
  let lex_result = lex s in
  match lex_result with
  | Some tokens ->
    let parse_result = parse tokens in
    (match parse_result with
     | Some prog ->
       (match interp_coms prog [] with
        | Some (output, _) -> Some output
        | None -> None)
     | None -> None)
  | None -> None;;
