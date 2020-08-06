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

open Trace_type
open Dba

class trace_visitor_inplace = object(self)

  val mutable instruction_callback = fun off inst -> ()
  val mutable dbainstruction_callback = fun off inst dbainst -> ()

  val mutable not_decoded_callback = fun off inst -> ()
  val mutable syscall_instr_callback = fun off inst -> ()
  val mutable libcall_instr_callback = fun off inst -> ()

  val mutable dba_nonlocal_if = fun off inst dbainst -> ()
  val mutable dba_dynamic_jump = fun off inst dbainst -> ()
  val mutable dba_dynamic_call = fun off inst dbainst -> ()
  val mutable dba_dynamic_ret = fun off inst dbainst -> ()
  val mutable dba_dynamic_jump_strict = fun off inst dbainst -> ()

  val mutable metadata_callback = fun off meta -> ()
  val mutable exception_callback = fun off typ handler -> ()
  val mutable module_callback = fun off s -> ()
  val mutable wave_callback = fun off v -> ()

  method set_instruction_callback f = instruction_callback <- f
  method set_dbainstruction_callback f = dbainstruction_callback <- f

  method set_not_decoded_callback f = not_decoded_callback <- f
  method set_syscall_instr_callback f = syscall_instr_callback <- f
  method set_libcall_instr_callback f = libcall_instr_callback <- f

  method set_dba_nonlocal_if f = dba_nonlocal_if <- f
  method set_dba_dynamic_jump f = dba_dynamic_jump <- f
  method set_dba_dynamic_call f = dba_dynamic_call <- f
  method set_dba_dynamic_ret f = dba_dynamic_ret <- f
  method set_dba_dynamic_jump_strict f = dba_dynamic_jump_strict <- f

  method set_metadata_callback f = metadata_callback <- f
  method set_exception_callback f = exception_callback <- f
  method set_module_callback f = module_callback <- f
  method set_wave_callback f = wave_callback <- f

  method compute (trace:trace): unit =
    InstrMap.iter ( fun offset inst ->
        if Int.Map.mem offset trace.metadatas then
          List.iter (fun meta ->
              metadata_callback offset meta;
              match meta with
              | Exception(typ,handler) -> exception_callback offset typ handler
              | Module s -> module_callback offset s
              | Layer i -> wave_callback offset i
            ) (Int.Map.find offset trace.metadatas);
        instruction_callback offset inst;
        if not(inst.decoded) then not_decoded_callback offset inst;
        if Syscall_stub.is_syscall_opcode inst.opcode then syscall_instr_callback offset inst;
        if is_libcall inst.concrete_infos then libcall_instr_callback offset inst;
        List.iter (fun dbainst ->
            dbainstruction_callback offset inst dbainst;
            match snd dbainst with
            | DbaIf(_,JOuter(_), _) -> dba_nonlocal_if offset inst dbainst
            | DbaDJump(e,tgs) ->
              dba_dynamic_jump offset inst dbainst;
              begin match tgs with
                | Some(DbaCall(_)) -> dba_dynamic_call offset inst dbainst
                | Some(DbaReturn) -> dba_dynamic_ret offset inst dbainst
                | None -> dba_dynamic_jump_strict offset inst dbainst
              end
            | _ -> ()
          ) inst.dbainstrs
      ) trace.instrs

end;;
