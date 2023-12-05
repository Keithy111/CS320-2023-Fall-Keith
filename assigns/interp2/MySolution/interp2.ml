#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-2.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

type const =
  | Int of int 
  | Bool of bool 
  | Unit
  | Symbol of string

type closure = {
  body: coms;
  env: environment;
}
and environment = (string * const) list

and com =
  | Push of const
  | PushClosure of closure
  | Pop
  | Trace
  | Swap
  | Add
  | Sub
  | Mul 
  | Div
  | And 
  | Or 
  | Not
  | Lt 
  | Gt
  | IfElse of coms * coms  
  | Bind                   
  | Lookup                
  | Fun of string * coms    
  | Call                   
  | Return           

and coms = com list
type stack = const list
type trace = string list
type prog = coms
type env = (string * const) list

let parse_nat = let* n = natural << whitespaces in pure n
 
let parse_int =
  (let* n = parse_nat in pure (Int n)) <|>
  (keyword "-" >> let* n = parse_nat in pure (Int (-n)))
 
let parse_bool =
  (keyword "True" >> pure (Bool true)) <|>
  (keyword "False" >> pure (Bool false))
 
let parse_unit =
  keyword "Unit" >> pure Unit

(*
   char_escaped: Takes a character and returns its escaped representation.
   Handles specific escape cases for newline, tab, carriage return, backslash,
   and single quote characters. Defaults to using the character as is.
*)
let char_escaped c =
   match c with
   | '\n' -> "\\n"
   | '\t' -> "\\t"
   | '\r' -> "\\r"
   | '\\' -> "\\\\"
   | '\'' -> "\\'"
   | _    -> str c

(*
  get_char: Parses a sequence of characters satisfying the char_isletter predicate.
  Wraps the list of characters in a list_make_fwork construct.
*)
let get_char =
 let* chars = many (satisfy char_isletter) in
 pure (list_make_fwork (fun work -> list_foreach chars work))

(*
  parse_string: Parses a string of characters using the get_char parser.
  Escapes each character using char_escaped and concatenates them using list_foldleft.
*)
let parse_string : string parser =
 fun ls ->
   match get_char ls with
   | Some (charList, rest) ->
     Some (list_foldleft charList "" (fun acc c -> acc ^ get_char c), rest)
   | None -> None

(*
  parse_symbol: Parses a symbol using parse_string followed by whitespaces.
  Wraps the result in the Symbol constructor using pure.
*)
let parse_symbol =
 let* n = parse_string << whitespaces in pure (Symbol n)
 let parse_const =
   parse_int <|>
   parse_bool <|>
   parse_unit <|>
   parse_symbol

let rec parse_com () =
   (keyword "Pop" >> pure Pop) <|>
   (keyword "Trace" >>= fun () -> pure Trace) <|>
   (keyword "Swap" >> pure Swap) <|>
   (keyword "Add" >> pure Add) <|>
   (keyword "Sub" >> pure Sub) <|>
   (keyword "Mul" >> pure Mul) <|>
   (keyword "Div" >> pure Div) <|>
   (keyword "And" >> pure And) <|>
   (keyword "Or" >> pure Or) <|>
   (keyword "Not" >> pure Not) <|>
   (keyword "Lt" >> pure Lt) <|>
   (keyword "Gt" >> pure Gt) <|>
   (keyword "If" >> parse_coms () >>= fun c1 -> 
      keyword "Else" >> parse_coms () >>= fun c2 -> 
      keyword "End" >> pure (IfElse(c1, c2))) <|>  
   (keyword "Bind" >> pure Bind) <|>
   (keyword "Lookup" >> pure Lookup) <|>
   (keyword "Fun" >> 
      parse_symbol >>= (function 
                        | Symbol sym -> 
                           keyword "{" >> 
                           parse_coms () >>= fun body -> 
                           keyword "}" >> 
                           pure (Fun(sym, body))
                        | _ -> fail)) <|>  
   (keyword "Call" >> pure Call) <|>
   (keyword "Return" >> pure Return)
and parse_coms () =
   let* _ = pure () in 
   many (parse_com () << keyword ";")
    
let rec str_of_nat (n : int) : string =
   let d = n mod 10 in 
   let n0 = n / 10 in
   let s = str (chr (d + ord '0')) in 
   if 0 < n0 then
     string_append (str_of_nat n0) s
   else s
 
let str_of_int (n : int) : string = 
   if n < 0 then
      string_append "-" (str_of_nat (-n))
   else str_of_nat n
 
let toString (c : const) : string =
   match c with
   | Int i -> str_of_int i
   | Bool true -> "True"
   | Bool false -> "False"
   | Unit -> "Unit"
   | Symbol s -> s
   | Closure (s, v, p) -> 
      let s1 = string_append ("Fun<") (s) in
      string_append (s1) (">")

let assoc_opt key lst =
  let rec aux lst =
    match lst with
    | [] -> None
    | (k, v) :: tail -> if k = key then Some v else aux tail
  in
  aux lst

type stack_item =
  | Const of const
  | Closure of closure
  | Marker of stack * environment
and stack = stack_item list

let rec eval (s : stack) (t : trace) (v: env) (p : prog) : trace =
  match p with
  | [] -> t
  | Push c :: p0 -> eval (c :: s) t v p0
  | PushClosure closure :: p0 -> eval (Closure closure :: s) t v p0
  | Pop :: p0 ->
    (match s with
     | _ :: s0 -> eval s0 t v p0
     | [] -> eval [] ("Panic" :: t) v p0)  (* Handle empty stack *)
  | Trace :: p0 ->
    (match s with
     | c :: s0 -> let str_c = 
                    (match c with
                     | Const const_val -> toString const_val
                     | Closure _ -> "[closure]"
                     | Marker (_, _) -> "[marker]")  (* Added Marker case *)
                  in eval (Const Unit :: s0) (str_c :: t) v p0
     | [] -> eval [] ("Panic" :: t) v p0)
  | Swap :: p0 ->
    (match s with
     | x1 :: x2 :: s0 -> eval (x2 :: x1 :: s0) t v p0
     | _ -> eval [] ("Panic" :: t) v p0)
  | Add :: p0 ->
    (match s with
     | Const (Int i) :: Const (Int j) :: s0 -> eval (Const (Int (i + j)) :: s0) t v p0
     | _ :: _ :: s0 (* AddError1 *) -> eval [] ("Panic" :: t) v p0
     | []                   (* AddError2 *) -> eval [] ("Panic" :: t) v p0
     | _ :: []              (* AddError3 *) -> eval [] ("Panic" :: t) v p0)
  | Sub :: p0 ->
    (match s with
     | Const (Int i) :: Const (Int j) :: s0 (* SubStack *)  -> eval (Const (Int (i - j)) :: s0) t v p0
     | _ :: _ :: s0         (* SubError1 *) -> eval [] ("Panic" :: t) v p0
     | []                   (* SubError2 *) -> eval [] ("Panic" :: t) v p0
     | _ :: []              (* SubError3 *) -> eval [] ("Panic" :: t) v p0)
  | Mul :: p0 ->
    (match s with
     | Const (Int i) :: Const (Int j) :: s0 (* MulStack *)  -> eval (Const (Int (i * j)) :: s0) t v p0
     | _ :: _ :: s0         (* MulError1 *) -> eval [] ("Panic" :: t) v p0
     | []                   (* MulError2 *) -> eval [] ("Panic" :: t) v p0
     | _ :: []              (* MulError3 *) -> eval [] ("Panic" :: t) v p0)
  | Div :: p0 ->
    (match s with
     | Const (Int i) :: Const (Int 0) :: s0 (* DivError0 *) -> eval [] ("Panic" :: t) v p0
     | Const (Int i) :: Const (Int j) :: s0 (* DivStack *)  -> eval ((Const (Int (i / j))) :: s0) t v p0
     | _ :: _ :: s0         (* DivError1 *) -> eval [] ("Panic" :: t) v p0
     | []                   (* DivError2 *) -> eval [] ("Panic" :: t) v p0
     | _ :: []              (* DivError3 *) -> eval [] ("Panic" :: t) v p0)
  | And :: p0 ->
    (match s with
     | Const (Bool a) :: Const (Bool b) :: s0 -> eval (Const (Bool (a && b)) :: s0) t v p0
     | _ :: _ :: s0           (* AndError1 *) -> eval [] ("Panic" :: t) v p0
     | []                     (* AndError2 *) -> eval [] ("Panic" :: t) v p0
     | _ :: []                (* AndError3 *) -> eval [] ("Panic" :: t) v p0)
  | Or :: p0 ->
    (match s with
     | Const (Bool a) :: Const (Bool b) :: s0 -> eval (Const (Bool (a || b)) :: s0) t v p0
     | _ :: _ :: s0           (* OrError1 *) -> eval [] ("Panic" :: t) v p0
     | []                     (* OrError2 *) -> eval [] ("Panic" :: t) v p0
     | _ :: []                (* OrError3 *) -> eval [] ("Panic" :: t) v p0)
  | Not :: p0 ->
    (match s with
     | Const (Bool a) :: s0 (* NotStack  *) -> eval (Const (Bool (not a)) :: s0) t v p0
     | _ :: s0      (* NotError1 *) -> eval [] ("Panic" :: t) v p0
     | []           (* NotError2 *) -> eval [] ("Panic" :: t) v p0
     | _ :: []      (* NotError3 *) -> eval [] ("Panic" :: t) v p0)
  | Lt :: p0 ->
    (match s with
     | Const (Int i) :: Const (Int j) :: s0 -> eval (Const (Bool (i < j)) :: s0) t v p0
     | _ :: _ :: s0          (* LtError1 *) -> eval [] ("Panic" :: t) v p0
     | []                    (* LtError2 *) -> eval [] ("Panic" :: t) v p0
     | _ :: []               (* LtError3 *) -> eval [] ("Panic" :: t) v p0)
  | Gt :: p0 ->
    (match s with
     | Const (Int i) :: Const (Int j) :: s0 -> eval (Const (Bool (i > j)) :: s0) t v p0
     | _ :: _ :: s0          (* GtError1 *) -> eval [] ("Panic" :: t) v p0
     | []                    (* GtError2 *) -> eval [] ("Panic" :: t) v p0
     | _ :: []               (* GtError3 *) -> eval [] ("Panic" :: t) v p0)
  | IfElse (p1, p2) :: p0 ->
    (match s with
     | Const (Bool true) :: s0 -> eval s0 t v (p1 @ p0)
     | Const (Bool false) :: s0 -> eval s0 t v (p2 @ p0)
     | _ :: s0                 (* IfElseError *) -> eval [] ("Panic" :: t) v p0
     | []                      (* IfElseError *) -> eval [] ("Panic" :: t) v p0)
  | Bind :: p0 ->
    (match s with
     | Const (Symbol x) :: c :: s0 -> eval s0 t ((x, c) :: v) p0
     | _ :: _ :: s0            (* BindError1 *) -> eval [] ("Panic" :: t) v p0
     | []                      (* BindError2 *) -> eval [] ("Panic" :: t) v p0
     | _ :: []                 (* BindError3 *) -> eval [] ("Panic" :: t) v p0)
  | Lookup :: p0 ->
    (match s with
     | Const (Symbol x) :: s0 -> (match lookup x v with
                                 | Some c -> eval (c :: s0) t v p0
                                 | None -> eval [] ("Panic" :: t) v p0)
     | _ :: s0                (* LookupError *) -> eval [] ("Panic" :: t) v p0
     | []                     (* LookupError *) -> eval [] ("Panic" :: t) v p0)
  | Fun (x, p1) :: p0 ->
    let closure = { body = p1; env = v } in
    eval (Closure closure :: s) t v p0
  | Call :: p0 ->
    (match s with
     | Closure closure :: c :: s0 ->
       let new_env = (x, c) :: closure.env in
       eval [] t new_env (closure.body @ p0)
     | _ :: _ :: s0           (* CallError1 *) -> eval [] ("Panic" :: t) v p0
     | _ :: []                (* CallError2 *) -> eval [] ("Panic" :: t) v p0
     | []                     (* CallError3 *) -> eval [] ("Panic" :: t) v p0)
  | Return :: p0 ->
    (match s with
     | c :: s0 -> eval (c :: s0) t v p0
     | _ :: s0 (* ReturnError *) -> eval [] ("Panic" :: t) v p0
     | []       (* ReturnError *) -> eval [] ("Panic" :: t) v p0)
  (* YOUR CODE *)
  
let interp (s : string) : string list option =
   match string_parse (whitespaces >> parse_coms()) s with
   | Some (p, []) -> Some (eval [] [] [] p)
   | _ -> None
