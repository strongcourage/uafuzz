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

open Analysis_config_piqi.Po_analysis_results_po_data
open Dba_types


let get_alive_branches () =
  if !Options.opaque_predicates_file = ""
  then
    Caddress.Map.empty
  else
    let raw = File_utils.load !Options.opaque_predicates_file in
    let buf = Piqirun.init_from_string raw in
    let infos = Analysis_config_piqi.parse_po_analysis_results buf in
    (* Renvoie un Po_analysis_results (voir analysis_config_piqi.ml) *)
    let preds =
      List.filter (fun data -> data.ksteps = 16l) infos.Analysis_config_piqi.Po_analysis_results.values in
    (* garde les infos pour k=16 *)
    List.fold_left (fun acc pred ->
        match pred.status with
        | `opaque ->
          let addr = ((Bitvector.create (Bigint.big_int_of_int64 pred.jmp_addr) 32), 0) in
          let addr_branch =
            match pred.alive_branch with
              Some addr -> Some ((Bitvector.create (Bigint.big_int_of_int64 addr) 32), 0)
            | None -> None
          in
          Caddress.Map.add addr addr_branch acc
        | _ -> acc
      ) Caddress.Map.empty preds
