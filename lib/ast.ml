open Types
open Env

let rec assert_unique = function
  | [] -> ()
  | x :: xs ->
    if List.mem x xs then raise @@ UniqueError x else assert_unique xs

let rec build_ast sexp =
  let rec quasiquote = function
    | Pair (Symbol "unquote", Pair (e, Nil)) -> e
    | Pair (Symbol "unquote-splicing", _) ->
      raise (SyntaxError "splice must be inside a list")
    | Pair (a, b) when is_list (Pair (a, b)) ->
      let elems = pair_to_list (Pair (a, b)) in
      qq_list elems
    | other ->
      (* Atom: produce (quote other) *)
      Pair (Symbol "quote", Pair (other, Nil))
  and qq_list = function
    | [] -> Nil
    | Pair (Symbol "unquote-splicing", Pair (e, Nil)) :: rest ->
      (* splice: produce (append e rest-result) *)
      Pair (Symbol "append", Pair (e, Pair (qq_list rest, Nil)))
    | x :: xs ->
      let car = quasiquote x in
      let cdr = qq_list xs in
      Pair (Symbol "pair", Pair (car, Pair (cdr, Nil)))
  in
  let rec cond_to_if = function
    | [] -> Literal (Symbol "error")
    | Pair (cond, Pair (res, Nil)) :: condpairs ->
      If (build_ast cond, build_ast res, cond_to_if condpairs)
    | _ -> raise @@ TypeError "(cond c0 c1 c2 c3 ...)"
  in
  match sexp with
  | Primitive _ | Closure _ | Macro _ -> raise ThisCan'tHappenError
  | Fixnum _ | Boolean _ | Nil | Quote _ | String _ | Vector _ | Map _
  | Keyword _ ->
    Literal sexp
  | Symbol s -> Var s
  | Pair _ when is_list sexp ->
    (match pair_to_list sexp with
     | Symbol "if" :: [ cond; iftrue ] ->
       If (build_ast cond, build_ast iftrue, Literal Nil)
     | Symbol "if" :: [ cond; iftrue; iffalse ] ->
       If (build_ast cond, build_ast iftrue, build_ast iffalse)
     | Symbol "cond" :: conditions -> cond_to_if conditions
     | [ Symbol "and"; c1; c2 ] -> And (build_ast c1, build_ast c2)
     | [ Symbol "or"; c1; c2 ] -> Or (build_ast c1, build_ast c2)
     | [ Symbol "quote"; e ] -> Literal (Quote e)
     | [ Symbol "quasiquote"; e ] -> build_ast (Quote (quasiquote e))
     | [ Symbol "val"; Symbol n; e ] -> Defexp (Val (n, build_ast e))
     | Symbol "let" :: bindings :: body when body <> [] ->
       let mkbinding = function
         | Pair (Symbol n, Pair (e, Nil)) -> n, build_ast e
         | _ -> raise @@ TypeError "(let bindings ...body)"
       in
       let mkbinding_vec bs =
         let rec pairup = function
           | [] -> []
           | Symbol n :: e :: rest -> (n, build_ast e) :: pairup rest
           | _ -> raise @@ TypeError "(let [name val ...] body)"
         in
         pairup bs
       in
       let bindings =
         match bindings with
         | Vector v -> mkbinding_vec v
         | _ when is_list bindings ->
           List.map mkbinding (pair_to_list bindings)
         | _ -> raise @@ TypeError "(let bindings ...body)"
       in
       let () = assert_unique (List.map fst bindings) in
       let body_ast =
         match body with
         | [ e ] -> build_ast e
         | es -> Do (List.map build_ast es)
       in
       Let (bindings, body_ast)
     | [ Symbol "lambda"; ns; e ] ->
       let err () = raise @@ TypeError "(lambda (formals) body)" in
       let names =
         match ns with
         | Vector v -> List.map (function Symbol s -> s | _ -> err ()) v
         | _ when is_list ns ->
           List.map
             (function Symbol s -> s | _ -> err ())
             (pair_to_list ns)
         | _ -> err ()
       in
       Lambda (names, build_ast e)
     | Symbol "lambda" :: ns :: body when body <> [] ->
       let err () = raise @@ TypeError "(lambda formals body-expr ...)" in
       let names =
         match ns with
         | Vector v -> List.map (function Symbol s -> s | _ -> err ()) v
         | _ when is_list ns ->
           List.map
             (function Symbol s -> s | _ -> err ())
             (pair_to_list ns)
         | _ -> err ()
       in
       let body_ast =
         match body with
         | [ e ] -> build_ast e
         | es -> Do (List.map build_ast es)
       in
       Lambda (names, body_ast)
     | [ Symbol "fn"; Symbol n; ns; e ] ->
       let err () = raise @@ TypeError "(fn name formals body)" in
       let names =
         match ns with
         | Vector v -> List.map (function Symbol s -> s | _ -> err ()) v
         | Symbol s -> [ s ]
         | _ when is_list ns ->
           List.map
             (function Symbol s -> s | _ -> err ())
             (pair_to_list ns)
         | _ -> err ()
       in
       let () = assert_unique names in
       let lam = Lambda (names, build_ast e) in
       Defexp (Val (n, lam))
     | Symbol "fn" :: Symbol n :: ns :: body when body <> [] ->
       let err () = raise @@ TypeError "(fn name formals body-expr ...)" in
       let names =
         match ns with
         | Vector v -> List.map (function Symbol s -> s | _ -> err ()) v
         | Symbol s -> [ s ]
         | _ when is_list ns ->
           List.map
             (function Symbol s -> s | _ -> err ())
             (pair_to_list ns)
         | _ -> err ()
       in
       let () = assert_unique names in
       let body_ast =
         match body with
         | [ e ] -> build_ast e
         | es -> Do (List.map build_ast es)
       in
       let lam = Lambda (names, body_ast) in
       Defexp (Val (n, lam))
     | [ Symbol "macro"; Symbol n; ns; e ] ->
       let err () = raise @@ TypeError "(macro name ...)" in
       let names =
         match ns with
         | Vector v -> List.map (function Symbol s -> s | _ -> err ()) v
         | Symbol s -> [ s ]
         | _ when is_list ns ->
           List.map
             (function Symbol s -> s | _ -> err ())
             (pair_to_list ns)
         | _ -> err ()
       in
       let () = assert_unique names in
       Defexp (Val (n, Literal (Macro (names, e, []))))
     | [ Symbol "apply"; fnexp; args ] ->
       Apply (build_ast fnexp, build_ast args)
     | fnexp :: args -> Call (build_ast fnexp, List.map build_ast args)
     | [] -> raise @@ ParseError "poorly formed expression")
  | Pair _ -> Literal sexp
