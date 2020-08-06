module VA = Virtual_address

val to_vaddr : string -> VA.t
val remove_from_right : 'a list -> 'a list
val strip_enclosing_chars : string -> string
val parse_calls : string -> (VA.t * VA.t * VA.t) list
val clean_mnemonic : string -> string
val to_supported : VA.t -> Mnemonic.t -> Mnemonic.t
val read_list : string -> string list

module Dot : sig
  val pp_id : Format.formatter -> Graph.Dot_ast.id -> unit
  val pp_a : Format.formatter ->
    Graph.Dot_ast.id * Graph.Dot_ast.id option -> unit
  val pp_attr : Format.formatter ->
    (Graph.Dot_ast.id * Graph.Dot_ast.id option) list -> unit
  val pp_attrs : Format.formatter ->
    (Graph.Dot_ast.id * Graph.Dot_ast.id option) list list -> unit
  val pp_node_id : Format.formatter -> Graph.Dot_ast.id * 'a -> unit
  val pp_node : Format.formatter -> Graph.Dot_ast.node -> unit
  val pp_stmt : Format.formatter -> Graph.Dot_ast.stmt -> unit
end
