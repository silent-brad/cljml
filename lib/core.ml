exception SyntaxError of string
exception NotFound of string
exception TypeError of string
exception ThisCan'tHappenError

type stream =
  { mutable line_num : int
  ; mutable chr : char list
  ; chan : in_channel
  }

type lobject =
  | Fixnum of int
  | Boolean of bool
  | Symbol of string
  | Nil
  | Pair of lobject * lobject

let rec lookup (n, e) =
  match e with
  | Nil -> raise (NotFound n)
  | Pair (Pair (Symbol n', v), rst) -> if n = n' then v else lookup (n, rst)
  | _ -> raise ThisCan'tHappenError

let bind (n, v, e) = Pair (Pair (Symbol n, v), e)

let rec pair_to_list pr =
  match pr with
  | Nil -> []
  | Pair (a, b) -> a :: pair_to_list b
  | _ -> raise ThisCan'tHappenError

let rec is_list e =
  match e with Nil -> true | Pair (a, b) -> is_list b | _ -> false

let rec eval_sexp sexp env =
  let eval_if cond iftrue iffalse =
    let condval, _ = eval_sexp cond env in
    match condval with
    | Boolean true -> iftrue
    | Boolean false -> iffalse
    | _ -> raise (TypeError "(if bool el1 el2)")
  in
  match sexp with
  | Fixnum v -> Fixnum v, env
  | Boolean v -> Boolean v, env
  | Symbol name -> lookup (name, env), env
  | Nil -> Nil, env
  | Pair (_, _) when is_list sexp ->
    (match pair_to_list sexp with
     | [ Symbol "if"; cond; iftrue; iffalse ] ->
       eval_sexp (eval_if cond iftrue iffalse) env
     | [ Symbol "pair"; car; cdr ] -> Pair (car, cdr), env
     | [ Symbol "env" ] -> env, env
     | [ Symbol "val"; Symbol name; exp ] ->
       let expval, _ = eval_sexp exp env in
       let env' = bind (name, expval, env) in
       expval, env'
     | _ -> sexp, env)
  | _ -> sexp, env
