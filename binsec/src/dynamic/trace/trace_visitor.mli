(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016                                                    *)
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

(** Visitor for traces*)

type trace_inst_callback = int -> Trace_type.trace_inst -> unit
type trace_inst_dba_callback = int -> Trace_type.trace_inst -> Dba_types.Statement.t -> unit

(** Classe used to visit and iterate a trace
    in a syntactic way (without DSE) *)
class trace_visitor_inplace :
  object

    method set_instruction_callback: trace_inst_callback -> unit (** set a callback on an instruction *)
    method set_dbainstruction_callback: trace_inst_dba_callback -> unit  (** set a callback on a DBA instruction *)

    method set_not_decoded_callback:  trace_inst_callback -> unit (** set a callback on a not decoded instruction *)
    method set_syscall_instr_callback: trace_inst_callback -> unit (** set a callback on a syscall *)
    method set_libcall_instr_callback: trace_inst_callback -> unit (** set a callback on a library call *)

    method set_dba_nonlocal_if: trace_inst_dba_callback -> unit (** set a callback on a DBA conditional instruction *)
    method set_dba_dynamic_jump: trace_inst_dba_callback -> unit (** set a callback on a DBA dynamic jump/call *)
    method set_dba_dynamic_call: trace_inst_dba_callback -> unit (** set a callback on a DBA dynamic call *)
    method set_dba_dynamic_ret: trace_inst_dba_callback -> unit (** set a callback on a DBA ret instruction *)
    method set_dba_dynamic_jump_strict: trace_inst_dba_callback -> unit (** set a callback on DBA dynamic jump only! *)
    method set_metadata_callback: (int -> Trace_type.metadata -> unit) -> unit (** set a callback on a metadata *)
    method set_exception_callback:  (int -> int32 -> int64 -> unit) -> unit (** set a callback on an exception *)
    method set_module_callback: (int -> string -> unit) -> unit (** set a callback on a module metadata *)
    method set_wave_callback: (int -> int -> unit) -> unit (** set a callback on a self-modification wave *)

    method compute: Trace_type.trace -> unit (** start the iteration on the given trace *)

  end
