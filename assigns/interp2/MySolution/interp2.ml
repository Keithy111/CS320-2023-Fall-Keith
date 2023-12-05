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
  | Sym of string

type com =
  | Push of const
  | Pop
  | Swap
  | Trace
  | Add | Sub | Mul | Div
  | And | Or | Not
  | Lt | Gt
  | If of com list * com list  (* If-Else End *)
  | Bind | Lookup
  | Fun of com list            (* Function definition *)
  | Call | Return

type closure = {
  name: string;
  env: (string * value) list;
  body: com list;
}

and value =
   | Const of const
   | Closure of closure

type coms = com list

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
  get_char: Parses a sequence of characters satisfying the char_isletter predicate.
  Wraps the list of characters in a list_make_fwork construct.
*)
let get_char =
 let* chars = many (satisfy char_isletter) in
 pure (list_make_fwork (fun work -> list_foreach chars work))

(*
  parse_string: Parses a string of characters using the get_char parser.
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

(* parsing commands *)
let rec parse_com () =
   parse_if_else () <|>
   (keyword "Push" >> parse_const >>= fun c -> pure (Push c)) <|>
   (keyword "Pop" >> pure Pop) <|>
   (keyword "Swap" >> pure Swap) <|>
   (keyword "Trace" >> pure Trace) <|>
   (keyword "Add" >> pure Add) <|>
   (keyword "Sub" >> pure Sub) <|>
   (keyword "Mul" >> pure Mul) <|>
   (keyword "Div" >> pure Div) <|>
   (keyword "And" >> pure And) <|>
   (keyword "Or" >> pure Or) <|>
   (keyword "Not" >> pure Not) <|>
   (keyword "Lt" >> pure Lt) <|>
   (keyword "Gt" >> pure Gt) <|>
   (keyword "Bind" >> pure Bind) <|>
   (keyword "Lookup" >> pure Lookup) <|>
   (keyword "Fun" >> parse_coms () << keyword "End" >>= fun cmds -> pure (Fun cmds)) <|>
   (keyword "Call" >> pure Call) <|>
   (keyword "Return" >> pure Return)

and parse_if_else () =
   let* _ = keyword "If" in
   let* c1 = parse_coms () in
   let* _ = keyword "Else" in
   let* c2 = parse_coms () in
   let* _ = keyword "End" in
   pure (If (c1, c2))
   
and parse_coms () = many (parse_com () << keyword ";")

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
 
 and toString (c : const) : string =
   match c with
   | Int i -> string_of_int i
   | Bool b -> string_of_bool b
   | Unit -> "Unit"
   | Sym s -> s

and loopThrough (sym: string) (env: (string * value) list) : value option =
   match env with
   | [] -> None
   | (s, v) :: rest ->
       if s = sym then Some v
       else loopThrough sym rest

let rec eval (stack : value list) (trace : string list) (env : (string * value) list) (prog : coms) : string list =
   match prog with
   | [] -> trace  (* termination returns trace *)
   | Push c :: p0 (* push stack *) -> eval ((Const c) :: stack) trace env p0
   | Pop :: p0 -> (
       match stack with
       | _ :: s0  (* PopStack *) -> eval s0 trace env p0
       | []       (* PopError *) -> "Panic" :: trace)
   | Swap :: p0 -> (
       match stack with
       | a :: b :: s0 (* SwapStack *) -> eval (b :: a :: s0) trace env p0
       | _            (* SwapError *) -> "Panic" :: trace)  
   | Trace :: p0 -> (
       match stack with
       | Const c :: s0 (* TraceStack *) -> eval (Const Unit :: s0) (toString c :: trace) env p0
       | _             (* TraceError *) -> "Panic" :: trace)  
   (* Add, Sub, Mul, Div *)
   | Add :: p0 -> (
      match stack with
      | Const (Int i) :: Const (Int j) :: s0 (* AddStack *)  -> eval (Const (Int (i + j)) :: s0) trace env p0
      | _ :: _ :: s0                         (* AddError1 *) -> "Panic" :: trace
      | []                                   (* AddError2 *) -> "Panic" :: trace
      | _ :: []                              (* AddError3 *) -> "Panic" :: trace)
   | Sub :: p0 -> (
      match stack with
      | Const (Int i) :: Const (Int j) :: s0 (* SubStack *)  -> eval (Const (Int (i - j)) :: s0) trace env p0
      | _ :: _ :: s0                         (* SubError1 *) -> "Panic" :: trace
      | []                                   (* SubError2 *) -> "Panic" :: trace
      | _ :: []                              (* SubError3 *) -> "Panic" :: trace)
   | Mul :: p0 -> (
      match stack with
      | Const (Int i) :: Const (Int j) :: s0 (* MulStack *)  -> eval (Const (Int (i * j)) :: s0) trace env p0
      | _ :: _ :: s0                         (* MulError1 *) -> "Panic" :: trace
      | []                                   (* MulError2 *) -> "Panic" :: trace
      | _ :: []                              (* MulError3 *) -> "Panic" :: trace)
   | Div :: p0 -> (
      match stack with
      | Const (Int i) :: Const (Int 0) :: s0 (* DivError0 *) -> "Panic" :: trace
      | Const (Int i) :: Const (Int j) :: s0 (* DivStack *)  -> eval (Const (Int (i / j)) :: s0) trace env p0
      | _ :: _ :: s0                         (* DivError1 *) -> "Panic" :: trace
      | []                                   (* DivError2 *) -> "Panic" :: trace
      | _ :: []                              (* DivError3 *) -> "Panic" :: trace)
   (* And, Or, Not *)
   | And :: p0 -> (
      match stack with
      | Const (Bool a) :: Const (Bool b) :: s0 (* AndStack *)  -> eval (Const (Bool (a && b)) :: s0) trace env p0
      | _ :: _ :: s0                           (* AndError1 *) -> "Panic" :: trace
      | []                                     (* AndError2 *) -> "Panic" :: trace
      | _ :: []                                (* AndError3 *) -> "Panic" :: trace)
   | Or :: p0 -> (
      match stack with
      | Const (Bool a) :: Const (Bool b) :: s0 (* OrStack *)  -> eval (Const (Bool (a || b)) :: s0) trace env p0
      | _ :: _ :: s0                           (* OrError1 *) -> "Panic" :: trace
      | []                                     (* OrError2 *) -> "Panic" :: trace
      | _ :: []                                (* OrError3 *) -> "Panic" :: trace)
   | Not :: p0 -> (
      match stack with
      | Const (Bool a) :: s0 (* NotStack *)  -> eval (Const (Bool (not a)) :: s0) trace env p0
      | _ :: s0              (* NotError1 *) -> "Panic" :: trace
      | []                   (* NotError2 *) -> "Panic" :: trace)
   (* Lt, Gt *)
   | Lt :: p0 -> (
      match stack with
      | Const (Int i) :: Const (Int j) :: s0 (* LtStack *)  -> eval (Const (Bool (i < j)) :: s0) trace env p0
      | _ :: _ :: s0                         (* LtError1 *) -> "Panic" :: trace
      | []                                   (* LtError2 *) -> "Panic" :: trace
      | _ :: []                              (* LtError3 *) -> "Panic" :: trace)
   | Gt :: p0 -> (
      match stack with
      | Const (Int i) :: Const (Int j) :: s0 (* GtStack *)  -> eval (Const (Bool (i > j)) :: s0) trace env p0
      | _ :: _ :: s0                         (* GtError1 *) -> "Panic" :: trace
      | []                                   (* GtError2 *) -> "Panic" :: trace
      | _ :: []                              (* GtError3 *) -> "Panic" :: trace)
   (* If then Else *)
   | If (c1, c2) :: p0 -> (
      match stack with
      | Const (Bool true) :: s0  (* IfStack *)      -> eval s0 trace env (c1 @ p0)
      | Const (Bool false) :: s0 (* ElseStack *)    -> eval s0 trace env (c2 @ p0)
      | _ :: s0                  (* IfElseError1 *) -> "Panic" :: trace
      | []                       (* IfElseError2 *) -> "Panic" :: trace)
   (* Bind, Lookup *)
   | Bind :: p0 -> (
      match stack with
      | Const (Sym x) :: v :: s0 (* BindStack *)  -> eval s0 trace ((x, v) :: env) p0
      | _ :: _ :: s0             (* BindError1 *) -> "Panic" :: trace 
      | []                       (* BindError2 *) -> "Panic" :: trace
      | _ :: []                  (* BindError3 *) -> "Panic" :: trace)
   | Lookup :: p0 -> (
      match stack with
      | Const (Sym x) :: s0 -> (
          match loopThrough x env with  
          | Some v (* LookupStack *)  -> eval (v :: s0) trace env p0
          | None   (* LookupError3 *) -> "Panic" :: trace)  
      | _ :: s0    (* LookupError1 *) -> "Panic" :: trace
      | []         (* LookupError2 *) -> "Panic" :: trace)

   (* Function, Call, Return *)
   | Fun cmds :: p0 -> (
      match stack with
      | Const (Sym f) :: s0 -> (* FunStack *)
        let closure = { name = f; env = env; body = cmds } in
        eval (Closure closure :: s0) trace env p0
      | _ :: s0 (* FunError1 *) -> "Panic" :: trace 
      | []      (* FunError2 *) -> "Panic" :: trace)

   | Call :: p0 -> (
      match stack with
      | Closure { name = f; env = closure_env; body = body_cmds } :: arg :: s0 -> (* CallStack *)
         let new_env: (string * value) list = (f, Closure { name = f; env = closure_env; body = body_cmds }) :: closure_env in
         eval (arg :: Closure { name = "cc"; env = env; body = p0 } :: s0) trace new_env body_cmds
      | _ :: _ :: s0 (* CallError1 *) -> "Panic" :: trace 
      | []           (* CallError2 *) -> "Panic" :: trace 
      | _ :: []      (* CallError3 *) -> "Panic" :: trace)

   | Return :: p0 -> (
      match stack with
      | Closure { name = _; env = closure_env; body = body_cmds } :: arg :: s0 -> (* RetStack *)
        eval (arg :: s0) trace closure_env body_cmds
      | _ :: _ :: s0 (* ReturnError1 *) -> "Panic" :: trace 
      | []           (* ReturnError2 *) -> "Panic" :: trace 
      | _ :: []      (* ReturnError3 *) -> "Panic" :: trace)

(* YOUR CODE *)
let interp (s : string) : string list option =
   match string_parse (whitespaces >> parse_coms()) s with
   | Some (p, []) -> Some (eval [] [] [] p)
   | _ -> None
