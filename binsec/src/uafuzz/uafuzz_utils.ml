module F = Ida_cfg.Function
module ICG = Ida_cg

module T = struct
  type t =
    | Alloc
    | Free
    | Use
    | Normal

  let of_string = function
    | "alloc" -> Alloc
    | "free" -> Free
    | "use" | "DF" -> Use
    | _ -> Normal
  ;;

  let to_string = function
    | Alloc -> "alloc"
    | Free -> "free"
    | Use -> "use"
    | Normal -> "normal"
  ;;

  let pp ppf = function
    | Alloc -> Format.fprintf ppf "A"
    | Free -> Format.fprintf ppf "F"
    | Use -> Format.fprintf ppf "U"
    | Normal -> Format.fprintf ppf "N"
  ;;

  let compare t1 t2 = Pervasives.compare t1 t2 ;;
end

(* heuristic to find alloc/free functions based on function's name *)
let event_funcs cg ~typ =
  ICG.fold_vertex (fun v acc ->
      let v_fname = ICG.Node.func (ICG.V.addr v) in
      if String_utils.contains
          (T.to_string typ) (F.to_string v_fname) then
        v_fname :: acc
      else acc
    ) cg []
;;

(* find all reachable functions to target function, TODO: optimize *)
let reachable_funcs cg ftarget =
  List.fold_left (fun acc e ->
      match ICG.node_from_fname cg e with
      | Some ev ->
        let ff =
          ICG.fold_vertex (fun f facc ->
            if ICG.is_reachable cg f ev then begin
              let ftyp = ICG.Node.typ (ICG.V.addr f) in
              if not (List.mem f acc) && ftyp == ICG.Node.T.Text then
                f :: facc
              else facc
            end
            else facc
          ) cg [] in
        ff @ acc
      | None -> acc
    ) [] [ftarget]
;;
