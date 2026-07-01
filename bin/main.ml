(* Make a Lisp *)

open Mal
open Mal.Core

let read_char stm =
  match stm.chr with
  | [] ->
    let c = input_char stm.chan in
    if c = '\n'
    then (
      let _ = stm.line_num <- stm.line_num + 1 in
      c)
    else c
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
    if is_digit nc
    then read_fixnum (acc ^ Char.escaped nc)
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
    if is_delimiter nc
    then (
      let _ = unread_char stm nc in
      "")
    else string_of_char nc ^ read_symbol ()
  in
  let rec read_list stm =
    eat_whitespace stm;
    let c = read_char stm in
    if c = ')'
    then Core.Nil
    else (
      let _ = unread_char stm c in
      let car = read_sexp stm in
      let cdr = read_list stm in
      Core.Pair (car, cdr))
  in
  eat_whitespace stm;
  let c = read_char stm in
  if is_symstartchar c
  then Core.Symbol (string_of_char c ^ read_symbol ())
  else if c = '('
  then read_list stm
  else if is_digit c || c = '~'
  then read_fixnum (Char.escaped (if c = '~' then '-' else c))
  else if c = '#'
  then (
    match read_char stm with
    | 't' -> Core.Boolean true
    | 'f' -> Core.Boolean false
    | x ->
      raise (Core.SyntaxError ("Invalid boolean literal " ^ Char.escaped x)))
  else raise (Core.SyntaxError ("Unexpected char " ^ Char.escaped c))

let rec print_sexp e =
  let rec print_list l =
    match l with
    | Core.Pair (a, Nil) -> print_sexp a
    | Core.Pair (a, b) -> print_sexp a; print_string " "; print_list b
    | _ -> raise Core.ThisCan'tHappenError
  in
  let print_pair p =
    match p with
    | Core.Pair (a, b) -> print_sexp a; print_string ". "; print_sexp b
    | _ -> raise Core.ThisCan'tHappenError
  in
  match e with
  | Core.Fixnum v -> print_int v
  | Core.Boolean b -> print_string (if b then "#t" else "#f")
  | Core.Symbol s -> print_string s
  | Core.Nil -> print_string "nil"
  | Core.Pair (a, b) ->
    print_string "(";
    if Core.is_list e then print_list e else print_pair e;
    print_string ")"

let rec repl stm env =
  print_string "> ";
  flush stdout;
  let sexp = read_sexp stm in
  let result, env' = Core.eval_sexp sexp env in
  print_sexp result; print_newline (); repl stm env'

let main =
  let stm = { chr = []; line_num = 1; chan = stdin } in
  repl stm Nil
