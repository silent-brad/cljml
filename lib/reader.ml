open Types
open Lwt.Syntax

let mkstream is_stdin stm = { chr = []; line_num = 1; is_stdin; stm }
let mkstringstream s = mkstream false (Lwt_stream.of_string s)

let mkfilestream f =
  if f = stdin then (
    let get () = Lwt_io.read_char_opt Lwt_io.stdin in
    mkstream true (Lwt_stream.from get))
  else
    mkstream false (Lwt_stream.of_string (In_channel.input_all f))

let read_char stm =
  match stm.chr with
  | [] ->
    let* c = Lwt_stream.get stm.stm in
    (match c with
     | None -> Lwt.fail End_of_file
     | Some c ->
       if c = '\n' then (
         stm.line_num <- stm.line_num + 1;
         Lwt.return c)
       else
         Lwt.return c)
  | c :: rest ->
    stm.chr <- rest;
    Lwt.return c

let unread_char stm c = stm.chr <- c :: stm.chr
let is_white c = c = ' ' || c = '\t' || c = '\n'

let rec eat_whitespace stm =
  let* c = read_char stm in
  if is_white c then eat_whitespace stm else Lwt.return (unread_char stm c)

let string_of_char c = String.make 1 c

let rec read_sexp stm =
  let is_digit c =
    let code = Char.code c in
    code >= Char.code '0' && code <= Char.code '9'
  in
  let rec read_fixnum acc =
    let* nc = read_char stm in
    if is_digit nc then
      read_fixnum (acc ^ Char.escaped nc)
    else (
      unread_char stm nc;
      Lwt.return (Fixnum (int_of_string acc)))
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
    let* nc = read_char stm in
    if is_delimiter nc then (
      unread_char stm nc; Lwt.return "")
    else
      let* rest = read_symbol () in
      Lwt.return (string_of_char nc ^ rest)
  in
  let rec read_list stm =
    let* () = eat_whitespace stm in
    let* c = read_char stm in
    if c = ')' then
      Lwt.return Nil
    else (
      unread_char stm c;
      let* car = read_sexp stm in
      let* cdr = read_list stm in
      Lwt.return (Pair (car, cdr)))
  in
  let rec eat_comment stm =
    let* c = read_char stm in
    if c = '\n' then Lwt.return () else eat_comment stm
  in
  let* () = eat_whitespace stm in
  let* c = read_char stm in
  if c = ';' then
    let* () = eat_comment stm in
    read_sexp stm
  else if is_symstartchar c then
    let* sym = read_symbol () in
    Lwt.return (Symbol (string_of_char c ^ sym))
  else if c = '(' then
    read_list stm
  else if is_digit c || c = '~' then
    read_fixnum (Char.escaped (if c = '~' then '-' else c))
  else if c = '#' then
    let* x = read_char stm in
    match x with
    | 't' -> Lwt.return (Boolean true)
    | 'f' -> Lwt.return (Boolean false)
    | x ->
      Lwt.fail @@ SyntaxError ("Invalid boolean literal " ^ Char.escaped x)
  else if c = '\'' then
    let* e = read_sexp stm in
    Lwt.return (Quote e)
  else
    Lwt.fail @@ SyntaxError ("Unexpected char " ^ Char.escaped c)
