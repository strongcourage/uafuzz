(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Formula_pp
open Lwt.Infix

module Command =
struct

  type t =
    | PutEntry of Formula.entry
    | CheckSat
    | GetModel
    | GetValue of Formula.term

  let pp ppf command =
    let open Format in
    match command with
    | PutEntry en -> fprintf ppf "@[%a@]" pp_entry en
    | CheckSat -> fprintf ppf "(check-sat)"
    | GetModel -> fprintf ppf "(get-model)"
    | GetValue tm ->
      fprintf ppf "@[<hov 2>(get-value@ (%a))@]" pp_term tm

  let print command =
    Print_utils.string_from_pp pp command

  let put_entry en = PutEntry en
  let check_sat = CheckSat
  let get_model = GetModel
  let get_value v = GetValue v
end


type t = {
  solver  : solver;
  process : Lwt_process.process;
  mutable buffer  : string list;
}

let get_solver t = t.solver

let rec read_model session list =
  Lwt.try_bind
    (fun () ->
       Lwt.pick
         [Lwt_unix.timeout 0.1;
          Lwt_io.read_line session.process#stdout])
    (fun string -> read_model session (string :: list))
    (function
      | Lwt_unix.Timeout -> Lwt.return (String.concat " " (List.rev list))
      | exn -> Lwt.fail exn)

let read_model session =
  Lwt_io.read_line session.process#stdout
  >>= fun string -> read_model session [string]
  (* some solvers sometimes forgets a parenthesis or two *)
  >|= Str.replace_first (Str.regexp "(((let (\\([^(]\\)") "(((let ((\\1"

let read_value session =
  read_model session
  >|= Lexing.from_string
  >|= Smtlib_parser.value Smtlib_lexer.token
  >|= Smt_model.extract_value

let read_model session =
  match session.solver with
  | Yices ->
    read_model session
    >|= Smt_model.yices_extract
  | _ ->
    read_model session
    >|= Lexing.from_string
    >|= Smtlib_parser.model Smtlib_lexer.token
    >|= Smt_model.extract

let read_status session =
  Lwt_io.read_line session.process#stdout
  >>= fun string ->
  match String.lowercase_ascii string with
  | "sat"     -> Lwt.return Formula.SAT
  | "unsat"   -> Lwt.return Formula.UNSAT
  | "unknown" -> Lwt.return Formula.UNKNOWN
  | "timeout" -> Lwt.return Formula.TIMEOUT
  | _ -> Lwt.fail_with ("Unknown status returned :" ^ string)

let write_command session command =
  session.buffer <- (Command.print command) :: session.buffer;
  Lwt.return_unit

let flush_command session =
  Lwt_io.write_lines session.process#stdin
    (Lwt_stream.of_list (List.rev session.buffer))
  >>= fun () -> session.buffer <- []; Lwt_io.flush session.process#stdin

let create ?timeout solver =
  let process =
    Lwt_process.open_process ?timeout
      (let p = Prover.command 0 solver in Command.(p.executable, p.arguments))
  in
  Lwt_io.write_line process#stdin "(set-logic QF_ABV)"
  >>= fun () -> Lwt.return { solver; process; buffer = [] }

let destroy session =
  session.process#terminate;
  session.process#close

let put_entry session entry =
  write_command session (Command.put_entry entry)

let check_sat session =
  write_command session Command.check_sat
  >>= fun () -> flush_command session
  >>= fun () -> read_status session

let get_model session =
  write_command session Command.get_model
  >>= fun () -> flush_command session
  >>= fun () -> read_model session

let get_value session term =
  write_command session (Command.get_value term)
  >>= fun () -> flush_command session
  >>= fun () -> read_value session
