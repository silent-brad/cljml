open Mal.Core
open Lwt.Syntax

let rec repl stm env =
  let* () =
    if stm.is_stdin then (
      print_string "> "; flush stdout; Lwt.return ())
    else
      Lwt.return ()
  in
  let* sexp = read_sexp stm in
  let result, env' = Eval.eval_sexp sexp env in
  let* () =
    if stm.is_stdin then (
      print_endline (string_val result);
      Lwt.return ())
    else
      Lwt.return ()
  in
  repl stm env'

let get_ic () = try open_in Sys.argv.(1) with Invalid_argument s -> stdin

let main =
  let ic = get_ic () in
  Lwt_main.run
    (Lwt.catch
       (fun () -> repl (mkfilestream ic) stdlib)
       (function
         | End_of_file ->
           if ic <> stdin then close_in ic;
           Lwt.return ()
         | e -> Lwt.fail e))
