include Cli.S

module AflInDirectory : Cli.STRING

module AflOutDirectory : Cli.STRING

module AflOutFile : Cli.STRING

module AflExtrasDir : Cli.STRING

module AflDeterminist : Cli.BOOLEAN

module AflTimeout : Cli.INTEGER

module AflMemLimit : Cli.STRING

module AflQemu : Cli.BOOLEAN

module AflCommand : Cli.STRING_OPT

type mode =
  | Fuzz
  | Showmap
val mode_to_string : mode -> string
module UAFuzzMode : Cli.GENERIC with type t = mode

module UAFuzzTargets : Cli.STRING

type input_metric =
  | AFLGoB
  | HawkeyeB
  | UAFuzz
val input_metric_to_string : input_metric -> string
module UAFuzzInputMetrics : Cli.GENERIC with type t = input_metric
