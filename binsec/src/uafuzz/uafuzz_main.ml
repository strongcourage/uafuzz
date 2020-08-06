open Uafuzz_options

module IF = Ida_cfg.Function
module UO = Uafuzz_options
module UT = Uafuzz_targets
module UM = Uafuzz_metrics

external uafuzz_c_main :
  Uafuzz_params.t -> cmd:string -> nb_params:int
  -> int = "uafuzz_main"

external uafuzz_c_showmap :
  Uafuzz_params.t -> cmd:string -> nb_params:int
  -> int = "uafuzz_showmap"

let uafuzz mode params ~cmd ~nb_params =
  match mode with
  | UO.Fuzz ->
    uafuzz_c_main params ~cmd ~nb_params
  | UO.Showmap ->
    uafuzz_c_showmap params ~cmd ~nb_params
;;

let run () =
  let ida_file = Ida_options.IdaOutputFile.get () in
  let ida_orig_file = Printf.sprintf "%s_orig" ida_file in
  let cg = Ida_main.parse_cg () in
  let g = Ida_main.parse_cfg ~simple:true ~ida_file in
  let g_orig = Ida_main.parse_cfg ~simple:true ~ida_file:ida_orig_file in
  let target_file = UO.UAFuzzTargets.get () in
  let fuzzer = UO.UAFuzzInputMetrics.get () in

  (* Distance metric for all directed fuzzers *)
  Unix.putenv "TARGETS_ENV_VAR" target_file;
  let target_uaf_file = Printf.sprintf "%s_uaf" target_file in
  Unix.putenv "UAF_ENV_VAR" target_uaf_file;
  let target_uaf = UT.from_file ~file:target_uaf_file in
  let targets_dist = UT.from_file ~file:target_file in
  Logger.result "targets dist: %a" UT.pp targets_dist;

  UM.Distance.update_cg_weights cg g_orig ~typ:fuzzer target_uaf;
  let (_, func_dist) = UM.Distance.flevel_distance cg g targets_dist in
  let fdist_file = Printf.sprintf "%s/func_distances.txt" (Sys.getcwd ()) in
  UM.Distance.to_fdist_file ~file:fdist_file func_dist;
  let bb_dists = UM.Distance.bblevel_distance cg g_orig targets_dist in
  let dist_file = Printf.sprintf "%s/distances.txt" (Sys.getcwd ()) in
  UM.Distance.to_bbdist_file ~file:dist_file bb_dists;

  (
    match fuzzer with
    | UO.UAFuzz ->
      (* Cut-edge metric only for UAFuzz *)
      let target_cut_file = Printf.sprintf "%s_cut" target_file in
      let targets_cut = UT.from_cut_file ~file:target_cut_file in
      Logger.result "targets cut: %a" UT.Pair.pp_list targets_cut;
      let cut_edges = UM.Cutedge.accumulate_cut_edges g targets_cut in
      let ce_file = Printf.sprintf "%s/cut_edges.txt" (Sys.getcwd ()) in
      UM.Cutedge.to_file ~file:ce_file cut_edges;
    | UO.HawkeyeB ->
      (* Trace closure only for Hawkeye *)
      let funcs_file = Printf.sprintf "%s/funcs.txt" (Sys.getcwd ()) in
      Unix.putenv "FUNCS_ENV_VAR" funcs_file;
      let target_funcs = UT.funcs targets_dist in
      Logger.result "target funcs: %a" IF.pp_list target_funcs;
      let trace_closure = UM.Trace_closure.targets_trace_closure cg target_funcs in
      let ttc_file = Printf.sprintf "%s/trace_closure.txt" (Sys.getcwd ()) in
      UM.Trace_closure.to_trace_closure_file ~file:ttc_file g trace_closure;
    | _ -> ()
  );

  (* Fuzzing *)
  let in_dir = UO.AflInDirectory.get () in
  let cmd, nb_params, params =
    Uafuzz_params.get_params ~in_dir ~targets:"target" in
  let mode = UO.UAFuzzMode.get () in
  let status = uafuzz mode params ~cmd ~nb_params in
  Logger.result "status: %d" status;
  exit 0
;;

let _ =
  Cli.Boot.enlist ~name:"UAFuzz" ~f:run;
;;
