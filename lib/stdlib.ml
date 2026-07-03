open Types
open Eval
open Ast
open Reader
open Lwt.Syntax

let stdlib =
  let ev env e =
    match e with
    | Defexp d -> evaldef d env
    | _ -> raise (TypeError "Can only have definitions in stdlib")
  in
  let rec slurp stm env =
    Lwt.catch
      (fun () ->
         let* sexp = read_sexp stm in
         let ast = build_ast sexp in
         let env' = ev env ast |> snd in
         slurp stm env')
      (function End_of_file -> Lwt.return env | e -> Lwt.fail e)
  in
  let stm_text =
    In_channel.with_open_text "stdlib.lsp" In_channel.input_all
  in
  let stm = mkstringstream stm_text in
  Lwt_main.run (slurp stm basis)
