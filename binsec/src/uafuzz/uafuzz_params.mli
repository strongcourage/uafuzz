type t

val get_params : in_dir:string -> targets:string ->
  Uafuzz_options.AflCommand.t * int * t
