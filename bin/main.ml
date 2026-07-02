(* Make a Lisp *)

open Mal
open Mal.Core

let read_char stm =
  match stm.chr with
  | [] ->
    let c = input_char stm.chan in
    if c = '\n' then (
      let _ = stm.line_num <- stm.line_num + 1 in
      c)
    else
      c
  | c :: rest ->
    let _ = stm.chr <- rest in
    c

let unread_char stm c = stm.chr <- c :: stm.chr
let is_white c = c = ' ' || c = '\t' || c = '\n'

let rec eat_whitespace stm =
  let c = read_char stm in
  if is_white c then eat_whitespace stm else unread_char stm c

let string_of_char c = String.make 1 c

let rec read_sexp stm =
  let is_digit c =
    let code = Char.code c in
    code >= Char.code '0' && code <= Char.code '9'
  in
  let rec read_fixnum acc =
    let nc = read_char stm in
    if is_digit nc then
      read_fixnum (acc ^ Char.escaped nc)
    else (
      let _ = unread_char stm nc in
      Core.Fixnum (int_of_string acc))
  in
  let is_symstartchar =
    let is_alpha = function 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false in
    function
    | '*' | '/' | '>' | '<' | '=' | '?' | '!' | '-' | '+' -> true
    | c -> is_alpha c
  in
  let rec read_symbol () =
    let is_delimiter = function
      | '(' | ')' | '|' | '{' | '}' | ';' -> true
      | c when c = '"' -> true
      | c -> is_white c
    in
    let nc = read_char stm in
    if is_delimiter nc then (
      let _ = unread_char stm nc in
      "")
    else
      string_of_char nc ^ read_symbol ()
  in
  let rec read_list stm =
    eat_whitespace stm;
    let c = read_char stm in
    if c = ')' then
      Core.Nil
    else (
      let _ = unread_char stm c in
      let car = read_sexp stm in
      let cdr = read_list stm in
      Core.Pair (car, cdr))
  in
  eat_whitespace stm;
  let c = read_char stm in
  if is_symstartchar c then
    Core.Symbol (string_of_char c ^ read_symbol ())
  else if c = '(' then
    read_list stm
  else if is_digit c || c = '~' then
    read_fixnum (Char.escaped (if c = '~' then '-' else c))
  else if c = '#' then (
    match
      read_char stm
    with
    | 't' -> Core.Boolean true
    | 'f' -> Core.Boolean false
    | x ->
      raise (Core.SyntaxError ("Invalid boolean literal " ^ Char.escaped x)))
  else if c = '\'' then
    Quote (read_sexp stm)
  else
    raise (Core.SyntaxError ("Unexpected char " ^ Char.escaped c))

let spacesep ns = String.concat " " ns

let rec string_exp = function
  | Core.Literal e -> string_val e
  | Core.Var n -> n
  | Core.If (c, t, f) ->
    "(if " ^ string_exp c ^ " " ^ string_exp t ^ " " ^ string_exp f ^ ")"
  | Core.And (c0, c1) -> "(and " ^ string_exp c0 ^ " " ^ string_exp c1 ^ ")"
  | Core.Or (c0, c1) -> "(or " ^ string_exp c0 ^ " " ^ string_exp c1 ^ ")"
  | Core.Apply (f, e) -> "(apply " ^ string_exp f ^ " " ^ string_exp e ^ ")"
  | Core.Call (f, es) ->
    let string_es = String.concat " " (List.map string_exp es) in
    "(" ^ string_exp f ^ " " ^ string_es ^ ")"
  | Core.Lambda (ns, e) -> "#<lambda>"
  | Core.Defexp (Val (n, e)) -> "(val " ^ n ^ " " ^ string_exp e ^ ")"
  | Defexp (Def (n, ns, e)) ->
    "(fn " ^ n ^ "(" ^ spacesep ns ^ ") " ^ string_exp e ^ ")"
  | Core.Defexp (Exp e) -> string_exp e

and string_val e =
  let rec string_list l =
    match l with
    | Core.Pair (a, Nil) -> string_val a
    | Core.Pair (a, b) -> string_val a ^ " " ^ string_list b
    | _ -> raise Core.ThisCan'tHappenError
  in
  let string_pair p =
    match p with
    | Core.Pair (a, b) -> string_val a ^ " . " ^ string_val b
    | _ -> raise Core.ThisCan'tHappenError
  in
  match e with
  | Core.Fixnum v -> string_of_int v
  | Core.Boolean b -> if b then "#t" else "#f"
  | Core.Symbol s -> s
  | Core.Nil -> "nil"
  | Core.Pair (a, b) ->
    "(" ^ (if is_list e then string_list e else string_pair e) ^ ")"
  | Core.Primitive (name, _) -> "#<primitive:" ^ name ^ ">"
  | Core.Quote v -> "'" ^ string_val v
  | Closure (ns, e, _) -> "#<closure>"

let rec repl stm env =
  print_string "> ";
  flush stdout;
  let ast = Core.build_ast (read_sexp stm) in
  let result, env' = Core.eval ast env in
  print_endline (string_val result);
  repl stm env'

let main =
  let stm = { chr = []; line_num = 1; chan = stdin } in
  repl stm Core.basis
