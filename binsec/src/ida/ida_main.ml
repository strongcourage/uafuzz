open Ida_options

module IO = Ida_options
module ICG = Ida_cg
module IC = Ida_cfg.C
module IG = Ida_cfg.G

let parse_cg () =
  let start = Sys.time () in
  let cg_file = Printf.sprintf "%s/callgraph.dot" (Sys.getcwd ()) in
  let cg = ICG.Parse.build_cg ~cg_file in
  let cg_time = Sys.time () in
  Logger.result "Parsing CG #nodes: %d, #edges: %d, time: %f (s)"
    (ICG.nb_vertex cg) (ICG.nb_edges cg) (cg_time -. start);
  cg
;;

let parse_cfg ~simple ~ida_file =
  let start = Sys.time () in
  let g = Ida_cfg.do_cfg ~simple ~ida_file in
  let cfg = IG.graph g in
  let cfg_time = Sys.time () in
  Logger.result "Parsing CFG #nodes: %d, #edges: %d, time: %f (s)"
    (IC.nb_vertex cfg) (IC.nb_edges cfg) (cfg_time -. start);
  g
;;

let run () =
  let simple = IO.IdaSimpleCfg.get () in
  let ida_file = IO.IdaOutputFile.get () in
  let _ = parse_cg () in
  let _ = parse_cfg ~simple ~ida_file in
  ()
;;

let _ =
  Cli.Boot.enlist ~name:"IDA + disassembly" ~f:run;
;;
