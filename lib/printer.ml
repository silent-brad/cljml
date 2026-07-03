open Types

let rec string_val e =
  let rec string_list l =
    match l with
    | Pair (a, Nil) -> string_val a
    | Pair (a, b) -> string_val a ^ " " ^ string_list b
    | _ -> raise ThisCan'tHappenError
  in
  let string_pair p =
    match p with
    | Pair (a, b) -> string_val a ^ " . " ^ string_val b
    | _ -> raise ThisCan'tHappenError
  in
  match e with
  | Fixnum v -> string_of_int v
  | Boolean b -> if b then "#t" else "#f"
  | Symbol s -> s
  | Nil -> "nil"
  | Pair (a, b) ->
    "(" ^ (if Env.is_list e then string_list e else string_pair e) ^ ")"
  | Primitive (name, _) -> "#<primitive:" ^ name ^ ">"
  | Quote v -> "'" ^ string_val v
  | Closure (ns, e, _) -> "#<closure>"

let spacesep ns = String.concat " " ns

let rec string_exp =
  let string_of_binding (n, e) = "(" ^ n ^ " " ^ string_exp e ^ ")" in
  function
  | Literal e -> string_val e
  | Var n -> n
  | If (c, t, f) ->
    "(if " ^ string_exp c ^ " " ^ string_exp t ^ " " ^ string_exp f ^ ")"
  | And (c0, c1) -> "(and " ^ string_exp c0 ^ " " ^ string_exp c1 ^ ")"
  | Or (c0, c1) -> "(or " ^ string_exp c0 ^ " " ^ string_exp c1 ^ ")"
  | Apply (f, e) -> "(apply " ^ string_exp f ^ " " ^ string_exp e ^ ")"
  | Call (f, es) ->
    let string_es = String.concat " " (List.map string_exp es) in
    "(" ^ string_exp f ^ " " ^ string_es ^ ")"
  | Lambda (ns, e) -> "#<lambda>"
  | Let (kind, bs, e) ->
    let str =
      match kind with LET -> "let" | LETSTAR -> "let*" | LETREC -> "letrec"
    in
    let bindings = spacesep (List.map string_of_binding bs) in
    "(" ^ str ^ " (" ^ bindings ^ ") " ^ string_exp e ^ ")"
  | Defexp (Val (n, e)) -> "(val " ^ n ^ " " ^ string_exp e ^ ")"
  | Defexp (Exp e) -> string_exp e
