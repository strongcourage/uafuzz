module VA = Virtual_address
module ICG = Ida_cg
module F = Ida_cfg.Function
module IG = Ida_cfg.G
module UT = Uafuzz_targets
module UO = Uafuzz_options

module Distance : sig
  val update_cg_weights : ICG.t -> IG.t -> typ:UO.input_metric -> UT.t -> unit
  val flevel_distance :
    ICG.t -> IG.t -> UT.t -> (F.t, float ref) Hashtbl.t * (VA.t, float ref) Hashtbl.t
  val bblevel_distance :
    ICG.t -> IG.t -> UT.t -> (VA.t, float ref) Hashtbl.t
  val to_bbdist_file : file:string ->
    (VA.t, float ref) Hashtbl.t -> unit
  val to_fdist_file : file:string ->
    (VA.t, float ref) Hashtbl.t -> unit
end

module Cutedge : sig
  module T : sig
    type t =
      | Cut
      | Non_cut
  end
  module Edge : sig
    type t
    val create : T.t -> VA.t -> VA.t -> t
    val pp : Format.formatter -> t -> unit
    val pp_list : Format.formatter -> t list -> unit
  end
  val accumulate_cut_edges : IG.t -> UT.Pair.t list -> Edge.t list
  val to_file : file:string -> Edge.t list -> unit
end

module Trace_closure : sig
  val targets_trace_closure : ICG.t -> F.t list -> ICG.Node.t list
  val to_trace_closure_file : file:string -> IG.t -> ICG.Node.t list -> unit
end
