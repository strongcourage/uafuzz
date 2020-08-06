module Node : sig
  module T : sig
    type t =
      | Entrypoint
      | Text
      | Plt
    val to_string : t -> string
    val pp : Format.formatter -> t -> unit
  end
  type t
  val nid : t -> Graph.Dot_ast.id
  val func : t -> Ida_cfg.Function.t
  val typ : t -> T.t
  val create : ?nid:Graph.Dot_ast.id ->
    Ida_cfg.Function.t -> T.t -> t
  val pp : Format.formatter -> t -> unit
  val pp_short : Format.formatter -> t -> unit
  val pp_list : Format.formatter -> t list -> unit
  val equal : t -> t -> bool
end

module Edge : sig
  type t
  val src : t -> Node.t
  val dst : t -> Node.t
  val create : Node.t -> Node.t -> t
  val pp : Format.formatter -> t -> unit
  val pp_list : Format.formatter -> t list -> unit
end

include Cfg.S with type addr = Node.t
               and type inst = Node.t
               and type symb = Node.t

module Parse : sig
  val build_cg : cg_file:string -> t
end

val is_reachable : t -> V.t -> V.t -> bool
val node_from_fname : t -> Ida_cfg.Function.t -> V.t option
