exception SyntaxError of string
exception ParseError of string
exception NotFound of string
exception TypeError of string
exception ThisCan'tHappenError
exception UnspecifiedValue of string
exception UniqueError of string

type 'a env = (string * 'a option ref) list

type stream =
  { mutable line_num : int
  ; mutable chr : char list
  ; is_stdin : bool
  ; stm : char Lwt_stream.t
  }

type lobject =
  | Fixnum of int
  | Boolean of bool
  | Symbol of string
  | Nil
  | Pair of lobject * lobject
  | Primitive of string * (lobject list -> lobject)
  | Quote of value
  | Closure of name list * exp * value env

and value = lobject
and name = string

and let_kind =
  | LET
  | LETSTAR
  | LETREC

and exp =
  | Literal of value
  | Var of name
  | If of exp * exp * exp
  | And of exp * exp
  | Or of exp * exp
  | Apply of exp * exp
  | Call of exp * exp list
  | Lambda of name list * exp
  | Let of let_kind * (name * exp) list * exp
  | Defexp of def

and def =
  | Val of name * exp
  | Exp of exp
