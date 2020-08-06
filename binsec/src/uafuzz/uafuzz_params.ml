open Uafuzz_options

type t = {
  in_dir : string; (*0*)
  out_dir : string; (*1*)
  out_file : string; (*2*)
  qemu : bool; (*3*)
  skip_det : bool; (*4*)
  timeout : int; (*5*)
  memlimit : string; (*6*)
  mode : Uafuzz_options.mode; (*7*)
  targets : string; (*8*)
  input_metrics : string; (*9*)
}

let create i o f q sd t mem mode targets im = {
  in_dir = i; out_dir = o; out_file = f;
  qemu = q; skip_det = sd; timeout = t; memlimit = mem;
  mode = mode; targets = targets; input_metrics = im;
}
;;

let get_params ~in_dir ~targets =
  if not (AflCommand.is_set ()) then
    raise (Failure "AFL Command?")
  else begin
    let cmd = AflCommand.get () in
    let nb_params =
      List.length (Str.split (Str.regexp_string " ") cmd) in
    let out_dir = AflOutDirectory.get () in
    let out_file = AflOutFile.get () in
    let qemu = AflQemu.get () in
    let skip_det = AflDeterminist.get () in
    let timeout = AflTimeout.get () in
    let memlimit = AflMemLimit.get () in
    let mode = UAFuzzMode.get () in
    let input_metrics = input_metric_to_string (UAFuzzInputMetrics.get ()) in
    let params = create in_dir out_dir out_file qemu skip_det
        timeout memlimit mode targets input_metrics in
    cmd, nb_params, params
  end
;;
