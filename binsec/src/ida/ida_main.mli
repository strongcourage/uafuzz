val parse_cg : unit -> Ida_cg.t

val parse_cfg : simple:bool -> ida_file:string -> Ida_cfg.G.t

val run : unit -> unit
