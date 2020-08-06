include Cli.Make(
 struct
   let shortname = "uafuzz"
   let name = "UAFuzz interface"
 end
)

module AflInDirectory = Builder.String(
    struct
      let name = "i"
      let default = "in"
      let doc = " Set input directory"
    end)

module AflOutDirectory = Builder.String(
    struct
      let name = "o"
      let default = "out"
      let doc = " Set output directory"
    end)

module AflOutFile = Builder.String(
    struct
      let name = "f"
      let default = "out_file"
      let doc = " Set output file [%s]"
    end)

module AflExtrasDir = Builder.String(
  struct
    let name = "x"
    let default = ""
    let doc = " Set optional fuzzer dictionary"
  end)

module AflDeterminist = Builder.False(
  struct
    let name = "d"
    let doc = " Skip deterministic stage"
  end)

module AflTimeout = Builder.Zero(
  struct
    let name = "to"
    let doc = " Set timeout (minutes) for fuzzing target reports"
  end)

module AflMemLimit = Builder.String(
  struct
    let name = "m"
    let default = "none"
    let doc = " Set memory limit"
  end)

module AflQemu = Builder.True(
  struct
    let name = "Q"
    let doc = " Set QEMU mode"
  end)

module AflCommand = Builder.String_option(
  struct
    let name = "r"
    let doc = " Set running command"
  end)

type mode =
  | Fuzz
  | Showmap

let mode_to_string = function
  | Fuzz -> "fuzz"
  | Showmap -> "showmap"
;;

module UAFuzzMode = Builder.Variant_choice_assoc(
  struct
    type t = mode
    let assoc_map = [
      "fuzz", Fuzz;
      "showmap", Showmap;
    ]
    let default = Fuzz
    let name = "M"
    let doc = " Set run mode"
  end)

module UAFuzzTargets = Builder.String(
  struct
    let name = "T"
    let default = "target"
    let doc = " Set UAF bug trace"
  end)

type input_metric =
  | AFLGoB (* binary version of AFLGo *)
  | HawkeyeB (* binary version of Hawkeye *)
  | UAFuzz

let input_metric_to_string = function
  | AFLGoB -> "aflgob"
  | HawkeyeB -> "heb"
  | UAFuzz -> "uafuzz"
;;

module UAFuzzInputMetrics = Builder.Variant_choice_assoc(
  struct
    type t = input_metric
    let assoc_map = [
      "aflgob", AFLGoB;
      "heb", HawkeyeB;
      "uafuzz", UAFuzz;
    ]
    let default = UAFuzz
    let name = "I"
    let doc = " Set input metrics"
  end)
