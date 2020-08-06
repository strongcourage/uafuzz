module IF = Ida_cfg.Function
module IG = Ida_cfg.G
module VA = Virtual_address

module Node = struct
  type t = {
    addr : VA.t; (* address of basic block *)
    func : IF.t;
  }

  let addr t = t.addr ;;
  let func t = t.func ;;

  let create a f = { addr = a; func = f; } ;;

  let pp ppf t =
    Format.fprintf ppf "%a:%a"
      IF.pp t.func VA.pp t.addr
  ;;

  let pp_list ppf t =
    Format.fprintf ppf "%a"
      (Print_utils.pp_list ~sep:", " pp) t
  ;;
end

module Pair = struct
  type t = {
    src : Node.t;
    dst : Node.t;
  }

  let src t = t.src ;;
  let dst t = t.dst ;;

  let create src dst = { src = src; dst = dst; } ;;

  let pp ppf t =
    Format.fprintf ppf "%a -> %a"
      Node.pp t.src Node.pp t.dst
  ;;

  let pp_list ppf t =
    Format.fprintf ppf "%a"
      (Print_utils.pp_list ~sep:"; " pp) t
  ;;
end

(* our representation of a target (in between DCT and slice) *)
type t = Node.t list

let pp ppf = Node.pp_list ppf ;;

let addrs t =
  List.fold_left (fun acc n -> (Node.addr n) :: acc) [] t

let funcs t =
  Ida_utils.remove_from_right
    (List.fold_left (fun acc n -> (Node.func n) :: acc) [] t)
;;

let from_file ~file =
  let read_file ~file =
    let lines = ref [] in
    let chan = open_in file in
    try
      while true; do
        lines := input_line chan :: !lines
      done; !lines
    with End_of_file ->
      close_in chan;
      !lines
  in
  List.fold_left (fun acc line ->
      let elements = String.split_on_char ',' line in
      let node = Node.create (VA.of_string (List.hd elements))
          (IF.name (List.nth elements 1)) in
      node :: acc
    ) [] (read_file ~file)
;;

let from_cut_file ~file =
  let read_file ~file =
    let lines = ref [] in
    let chan = open_in file in
    try
      while true; do
        lines := input_line chan :: !lines
      done; !lines
    with End_of_file ->
      close_in chan;
      !lines
  in
  List.fold_left (fun acc line ->
      let elements = String.split_on_char ';' line in
      let src = String.split_on_char ',' (List.hd elements) in
      let dst = String.split_on_char ',' (List.nth elements 1) in
      let node_src = Node.create (VA.of_string (List.hd src))
          (IF.name (List.nth src 1)) in
      let node_dst = Node.create (VA.of_string (List.hd dst))
          (IF.name (List.nth dst 1)) in
      let pair = Pair.create node_src node_dst in
      pair :: acc
    ) [] (read_file ~file)
;;
