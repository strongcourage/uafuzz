module T : sig
  type t =
    | Alloc
    | Free
    | Use
    | Normal
  val of_string : string -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val compare : t -> t -> int
end

val event_funcs : Ida_cg.t -> typ:T.t -> Ida_cfg.Function.t list
val reachable_funcs : Ida_cg.t -> Ida_cfg.Function.t ->
  Ida_cg.V.t list
