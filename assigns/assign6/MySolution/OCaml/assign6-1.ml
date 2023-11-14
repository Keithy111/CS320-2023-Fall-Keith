#use "./../../../../classlib/OCaml/MyOCaml.ml";;

(*
//
Assign6-1:
//
Please implement a print and parse function. Using parser combinators. When
given a valid string according to the grammar, your parse function returns an
sexpr value encoding the expression.

//
let sexpr_to_string (e : sexpr)  : string       = ...
let sexpr_parse     (s : string) : sexpr option = ...
//

Example (Accepted Strings):
sexpr_parse "(add 1 2 3)" = Some (SAdd [SInt 1; SInt 2; Int 3])
sexpr_parse "(mul (add 1 2) 3 (mul 1))" = Some (SMul [SAdd [SInt 1; SInt 2]; SInt 3; SMul [SInt 1]])
//
Example (Rejected Strings):
sexpr_parse "()" = None
sexpr_parse "(add)" = None
sexpr_parse "(add 1 2))" = None
sexpr_parse "((mul 1 2)" = None
//
*)

(* ****** ****** *)

(*

Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<exprs> ::= <expr> | <expr> <exprs>
<expr>  ::= <num>
          | (add <exprs> )
          | (mul <exprs> )
*)

type sexpr =
  | SInt of int        (* 1, 2, 3, 4 ...  *)
  | SAdd of sexpr list (* (add e1 e2 ...) *)
  | SMul of sexpr list (* (mul e1 e2 ...) *)

(* ****** ****** *)
let rec expr_to_sexpr (e: sexpr) : sexpr =
  match e with
  | SInt n -> SInt n
  | SAdd es -> SAdd (List.map expr_to_sexpr es)
  | SMul es -> SMul (List.map expr_to_sexpr es)

let rec sexpr_to_string (s: sexpr) : string =
  match s with
  | SInt n -> string_of_int n
  | SAdd es -> "(add " ^ String.concat " " (List.map sexpr_to_string es) ^ ")"
  | SMul es -> "(mul " ^ String.concat " " (List.map sexpr_to_string es) ^ ")"

let rec parse_sexpr () : sexpr parser =
  parse_sint () <|> parse_sadd () <|> parse_smul ()

and parse_sint () : sexpr parser =
  let* n = natural in
  pure (SInt n) << whitespaces

and parse_sadd () : sexpr parser =
  let* _ = keyword "(add" in
  let* es = many1' parse_sexpr in
  let* _ = keyword ")" in
  pure (SAdd es)

and parse_smul () : sexpr parser =
  let* _ = keyword "(mul" in
  let* es = many1' parse_sexpr in
  let* _ = keyword ")" in
  pure (SMul es)

let sexpr_parse (s : string) : sexpr option =
  match string_parse (parse_sexpr ()) s with
  | Some (e, []) -> Some e
  | _ -> None