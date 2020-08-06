module IF = Ida_cfg.Function
module IG = Ida_cfg.G
module VA = Virtual_address

module Node : sig
  type t
  val addr : t -> VA.t
  val func : t -> IF.t
  val create : VA.t -> IF.t -> t
  val pp : Format.formatter -> t -> unit
  val pp_list : Format.formatter -> t list -> unit
end

module Pair : sig
  type t
  val src : t -> Node.t
  val dst : t -> Node.t
  val create : Node.t -> Node.t -> t
  val pp : Format.formatter -> t -> unit
  val pp_list : Format.formatter -> t list -> unit
end

type t = Node.t list
val pp : Format.formatter -> t -> unit
val addrs : t -> VA.t list
val funcs : t -> IF.t list
val from_file : file:string -> t
val from_cut_file : file:string -> Pair.t list
