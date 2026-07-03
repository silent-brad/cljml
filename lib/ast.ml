open Types
open Env

let rec assert_unique = function
  | [] -> ()
  | x :: xs ->
    if List.mem x xs then raise @@ UniqueError x else assert_unique xs

let rec build_ast sexp =
  let rec cond_to_if = function
    | [] -> Literal (Symbol "error")
    | Pair (cond, Pair (res, Nil)) :: condpairs ->
      If (build_ast cond, build_ast res, cond_to_if condpairs)
    | _ -> raise @@ TypeError "(cond c0 c1 c2 c3 ...)"
  in
  let let_kinds = [ "let", LET; "let*", LETSTAR; "letrec", LETREC ] in
  let valid_let s = List.mem_assoc s let_kinds in
  let to_kind s = List.assoc s let_kinds in
  match sexp with
  | Primitive _ | Closure _ -> raise ThisCan'tHappenError
  | Fixnum _ | Boolean _ | Nil | Quote _ -> Literal sexp
  | Symbol s -> Var s
  | Pair _ when is_list sexp ->
    (match pair_to_list sexp with
     | [ Symbol "if"; cond; iftrue; iffalse ] ->
       If (build_ast cond, build_ast iftrue, build_ast iffalse)
     | Symbol "cond" :: conditions -> cond_to_if conditions
     | [ Symbol "and"; c1; c2 ] -> And (build_ast c1, build_ast c2)
     | [ Symbol "or"; c1; c2 ] -> Or (build_ast c1, build_ast c2)
     | [ Symbol "quote"; e ] -> Literal (Quote e)
     | [ Symbol "val"; Symbol n; e ] -> Defexp (Val (n, build_ast e))
     | [ Symbol s; bindings; exp ] when is_list bindings && valid_let s ->
       let mkbinding = function
         | Pair (Symbol n, Pair (e, Nil)) -> n, build_ast e
         | _ -> raise @@ TypeError "(let bindings exp)"
       in
       let bindings = List.map mkbinding (pair_to_list bindings) in
       let () = assert_unique (List.map fst bindings) in
       Let (to_kind s, bindings, build_ast exp)
     | [ Symbol "lambda"; ns; e ] when is_list ns ->
       let err () = raise @@ TypeError "(lambda (formals) body)" in
       let names =
         List.map (function Symbol s -> s | _ -> err ()) (pair_to_list ns)
       in
       Lambda (names, build_ast e)
     | [ Symbol "fn"; Symbol n; ns; e ] ->
       let err () = raise @@ TypeError "(fn name (formals) body)" in
       let names =
         List.map (function Symbol s -> s | _ -> err ()) (pair_to_list ns)
       in
       Defexp (Def (n, names, build_ast e))
     | [ Symbol "apply"; fnexp; args ] when is_list args ->
       Apply (build_ast fnexp, build_ast args)
     | fnexp :: args -> Call (build_ast fnexp, List.map build_ast args)
     | [] -> raise @@ ParseError "poorly formed expression")
  | Pair _ -> Literal sexp
