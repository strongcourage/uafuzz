open Uafuzz_options

val uafuzz : UAFuzzMode.t -> Uafuzz_params.t ->
  cmd:string -> nb_params:int -> int

val run : unit -> unit
