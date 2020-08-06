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

(** Non-blocking interface with SMT solvers *)

(* This module provides basic functions to solve SMT formulas, directly
 * interacting with the SMT solver via theirs incremental mode.
*)

type t

val create : ?timeout:float -> Formula_options.Solver.t -> t Lwt.t
val destroy : t -> Unix.process_status Lwt.t

val get_solver : t -> Formula_options.Solver.t

val put_entry : t -> Formula.entry -> unit Lwt.t
val check_sat : t -> Formula.status Lwt.t
val get_model : t -> Smt_model.t Lwt.t
val get_value : t -> Formula.term -> Bitvector.t Lwt.t
