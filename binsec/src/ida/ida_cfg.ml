include Ida_options

module VA = Virtual_address
module H = VA.Htbl
module IU = Ida_utils

module A = struct
  type t = VA.t
  let compare a1 a2 = compare a1 a2
  let hash (a: t) = (a :> int)
  let equal a1 a2 = a1 = a2
end

module I = struct
  type t = Instruction.t
  let hash (i: t) = (i.Instruction.address :> int)
  let equal i1 i2 = i1 = i2
end

module Function = struct
  type t =
    | Name of string
    | Address of VA.t

  let name s = Name s ;;
  let addr a = Address a ;;

  let pp ppf = function
    | Name s -> Format.fprintf ppf "%s" s
    | Address a -> VA.pp ppf a
  ;;

  let pp_list ppf t =
    Format.fprintf ppf "%a"
      (Print_utils.pp_list ~sep:", " pp) t
  ;;

  let same f1 f2 =
    match f1, f2 with
    | Name s1, Name s2 -> String.equal s1 s2
    | Address a1, Address a2  -> VA.equal a1 a2
    | _, _ -> false
  ;;

  let to_string = function
    | Name s -> s
    | Address a -> string_of_int (VA.to_int a)
  ;;

  let compare f1 f2 =
    match f1, f2 with
    | Name s1, Name s2 ->
      String.compare s1 s2
    | Address a1, Address a2 ->
      VA.compare a1 a2
    | _ -> raise (Failure "Not same type")
  ;;
end

module S = struct
  type t = {
    block : VA.t ;
    func  : Function.t;
  }

  let create block func = { block; func; }
  let block t = t.block
  let func t = t.func

  let equal = (=)
  let hash = Hashtbl.hash
end

module T = Cfg.Make(A)(I)(S)

module C = struct
  include T
  module PC = Graph.Path.Check(T)

  let fname_of_vertex v =
    match V.symb v with
    | None -> Function.name "unknown"
    | Some symb -> S.func symb
  ;;

  let decision_nodes g =
    fold_vertex (fun v acc ->
        if List.length (succ g v) > 1 then
          (V.addr v) :: acc
        else acc
      ) g []
  ;;

  let is_reachable g v1 v2 =
    let checker = PC.create g in
    PC.check_path checker v1 v2
  ;;

  let is_reachable_va g va1 va2 =
    match mem_vertex_a g va1 with
    | Some v1 ->
      begin
        match mem_vertex_a g va2 with
        | Some v2 -> is_reachable g v1 v2
        | None -> false
      end
    | None -> false
  ;;

  let exist_path g v source sink =
    is_reachable_va g source v &&
    is_reachable_va g v sink
  ;;

  module W = struct
    type edge = E.t
    type t = int
    let weight _x = 1
    let zero = 0
    let add = (+)
    let compare = compare
  end

  module Dij = Graph.Path.Dijkstra(T)(W)

  let shortest_len cfg va1 va2 =
    match mem_vertex_a cfg va1, mem_vertex_a cfg va2 with
    | Some v1, Some v2 ->
      let (_, w) = Dij.shortest_path cfg v1 v2 in
      Some (float_of_int w)
    | _, _ -> None
  ;;
end

module F = struct
  type t = {
    mutable eps : VA.Set.t;
    mutable name : string;
    blocks : VA.t list H.t;
    mutable edges : (VA.t * VA.t) list;
    calls : (VA.t * VA.t) list H.t;
  }

  let eps f = f.eps ;;
  (* assume function has only one entrypoint *)
  let one_ep f = List.hd (VA.Set.elements f.eps) ;;
  let name f = f.name ;;
  let blocks f = f.blocks ;;
  let edges f = f.edges ;;
  let calls f = f.calls ;;

  let create ~name ~edges ~eps =
    { eps; name; blocks = H.create 1000; edges; calls = H.create 1000; }
  ;;

  let empty () = create ~name:"" ~edges:[] ~eps:VA.Set.empty ;;

  (* Creates a function from a list of virtual addresses.
     This assumes the first item of the list of virtual addresses is the *lone*
     entry point of the function.
  *)
  let of_list ~name vas =
    let eps =
      match vas with
      | [] -> assert false
      | e :: _ -> VA.Set.singleton e in
    create ~name ~eps ~edges:[]
  ;;

  let all_leaders f = H.fold (fun k _ acc -> k :: acc) f.blocks [] ;;
  let all_insts f =
    let insts = H.fold (fun _ v acc -> v @ acc) f.blocks [] in
    List.sort (VA.compare) insts
  ;;

  let set_name f name = f.name <- name ;;
  let set_eps f eps = f.eps <- (VA.Set.union (f.eps) eps) ;;
  let set_blocks f k v = H.add f.blocks k v ;;
  let set_edges f e = f.edges <- e ;;
  let set_calls f k v = H.add f.calls k v ;;

  let leader f va =
    assert (List.mem va (all_insts f));
    List.hd (
      H.fold (fun va insts acc ->
          if List.mem va insts then va :: acc
          else acc
        ) f.blocks []
    )
  ;;

  let build_cfg f =
    let cfg = C.create 5000 in
    H.iter (fun v _insts -> C.add_addr cfg v) f.blocks;
    List.iter (fun (src, dst) ->
        C.add_edge_a cfg src dst) f.edges;
    cfg
  ;;

  let pp_edge ppf (s, d) =
    Format.fprintf ppf "%a -> %a"
      VA.pp s VA.pp d
  ;;

  let pp_edges ppf t =
    Format.fprintf ppf "%a"
      (Print_utils.pp_list ~sep:"\n" pp_edge) t.edges
  ;;

  let pp_va_list ppf addrs =
    List.iter (fun va -> Format.fprintf ppf "%a, " VA.pp va) addrs
  ;;

  let pp_blocks ppf t =
    H.iter (fun va insts ->
        Format.fprintf ppf "[%a: (%a)]\n"
          VA.pp va pp_va_list insts
      ) t.blocks
  ;;

  let pp_calls ppf t =
    let pp_call ppf (callee, ret) =
      Format.fprintf ppf "%a-%a" VA.pp callee VA.pp ret in
    let pp_list_calls ppf calls =
      Format.fprintf ppf "%a"
        (Print_utils.pp_list ~sep:", " pp_call) calls in
    H.iter (fun va calls ->
        Format.fprintf ppf "%a: [%a]\n"
          VA.pp va pp_list_calls calls;
      ) t.calls
  ;;

  let pp ppf f =
    Format.fprintf ppf "Function entrypoints {%a}:\n%a\n%a\n%a\n%a"
      pp_va_list (VA.Set.elements f.eps)
      pp_va_list (all_insts f)
      pp_blocks f pp_edges f pp_calls f
  ;;

  let nb_blocks f = H.length f.blocks ;;
  let nb_edges f = List.length f.edges ;;
  (* CC = E - V + 2 *)
  let cc f = (nb_edges f) - (nb_blocks f) + 2 ;;
end

module G = struct
  type t = {
    graph : C.t ;
    ep : VA.t option; (* entry point *)
    funcs : F.t H.t;
    calls : (VA.t * VA.t) list H.t ; (* caller->[callee-return] *)
  }

  let graph g = g.graph ;;
  let ep g = g.ep ;;
  let funcs g = g.funcs ;;
  let calls g = g.calls ;;

  let create ?ep () =
    { ep;
      graph = C.create 5000;
      funcs = H.create 5000;
      calls = H.create 5000;
    }
  ;;

  let pp ppf g =
    H.iter (fun _ f -> Format.fprintf ppf "%a\n" F.pp f) g.funcs;
  ;;

  let add_function t f =
    VA.Set.iter (fun va -> H.add t.funcs va f) (F.eps f);
  ;;

  let add_calls t ~caller ~callee ~return =
    match H.find t.calls caller with
    | cr ->
      H.replace t.calls caller ((callee, return) :: cr)
    | exception Not_found ->
      H.add t.calls caller [(callee, return)]
  ;;

  module V = C.V

  let _iter_edges_e f g = C.iter_edges_e f (graph g) ;;
  let iter_vertex  f g = C.iter_vertex  f (graph g) ;;
  let add_vertex g v = C.add_vertex (graph g) v ;;
  let _add_addr g = C.add_addr (graph g) ;;
  let add_inst g = C.add_inst (graph g) ;;
  let add_symb g = C.add_symb (graph g) ;;

  let _remove_vertex g = C.remove_vertex (graph g) ;;
  let _remove_addr g = C.remove_addr (graph g) ;;
  let _remove_inst g = C.remove_inst (graph g) ;;
  let _remove_symb g = C.remove_symb (graph g) ;;

  let _add_edge g = C.add_edge (graph g) ;;
  let _add_edge_e g = C.add_edge_e (graph g) ;;
  let add_edge_a g = C.add_edge_a (graph g) ;;

  let remove_edge g = C.remove_edge (graph g) ;;
  let _remove_edge_e g = C.remove_edge_e (graph g) ;;
  let _remove_edge_a g = C.remove_edge_a (graph g) ;;

  let _is_empty g = C.is_empty (graph g) ;;
  let _nb_vertex g = C.nb_vertex (graph g) ;;
  let _nb_edges g = C.nb_edges (graph g) ;;

  let _mem_vertex g   = C.mem_vertex (graph g)
  and mem_vertex_a g = C.mem_vertex_a (graph g) ;;

  let _mem_edge g   = C.mem_edge (graph g)
  and _mem_edge_a g = C.mem_edge_a (graph g)
  and _mem_edge_e g = C.mem_edge_e (graph g) ;;

  let succ g = C.succ (graph g)
  and pred g = C.pred (graph g) ;;

  let all_eps g =
    H.fold (fun _ f acc ->
        VA.Set.union acc (F.eps f)) g.funcs VA.Set.empty
  ;;

  let all_leaders g =
    H.fold (fun _ f acc ->
        VA.Set.union acc (VA.Set.of_list (F.all_leaders f))
      ) g.funcs VA.Set.empty
  ;;

  let fname_of_va g va =
    let f = H.fold (fun _ f acc ->
        if List.mem va (F.all_leaders f) then begin
          (Function.name (F.name f)) :: acc
        end
        else acc
      ) g.funcs [] in
    if List.length f = 1 then Some (List.hd f)
    else None
  ;;

  let disassemble_vertex g v =
    let addr = V.addr v in
    match V.inst v with
    | None -> ()
    | Some inst ->
      let opcode = Instruction.opcode inst in
      let mnemonic = Instruction.mnemonic inst in
      let inst', next =
        let base = VA.to_int addr in
        Disasm_core.decode_binstream ~base opcode in
      let dhunk = Instruction.hunk inst' in
      let mnemonic' = IU.to_supported addr mnemonic in
      Logger.debug "@[<hov 2>@[<h>%a %a %a @]@ %a@]"
        VA.pp addr
        Binstream.pp opcode
        Mnemonic.pp mnemonic'
        Dhunk.pp dhunk;
      let inst' =
        let open Instruction in
        set_dba_block dhunk inst |> set_mnemonic mnemonic' in
      if next <> None then
        Logger.debug "Next address: %a"
          (Print_utils.pp_opt VA.pp) next;
      add_inst g addr inst';
  ;;

  let has_dba_vertex g vaddr =
    match mem_vertex_a g vaddr with
    | None -> None
    | Some vaddr ->
      match V.inst vaddr with
      | None -> None
      | Some vinst ->
        let vdba = Instruction.hunk vinst in
        if not (Dhunk.is_empty vdba) then Some vaddr
        else None
  ;;

  let ret_nodes g f =
    List.fold_left (fun acc va ->
        match mem_vertex_a g va with
        | None -> acc
        | Some v ->
          if not (List.length (succ g v) = 0) then acc
          else
            (* no successors *)
            (match V.inst v with
             | None ->
               (* assume nodes of extern functions are return nodes *)
               va :: acc
             | Some v_inst ->
               let open Instruction in
               let opcode = Binstream.to_string (opcode v_inst) in
               let mnemonic = Mnemonic.to_string (mnemonic v_inst) in
               (* Logger.result "%s, %s" opcode mnemonic; *)
               (* conservatively check *)
               if String_utils.contains "ret" mnemonic &&
                  String_utils.contains "c3" opcode then
                 va :: acc
               else acc
            );
      ) [] (F.all_insts f)
  ;;

  (* TODO ICFG: add the return edges to G *)
  let add_ret_edges g =
    H.iter (fun _va calls ->
        List.iter (fun (callee_va, ret_va) ->
            let funcs = funcs g in
            (* callee_va is an entry point of callee function *)
            (match H.find funcs callee_va with
             | exception Not_found ->
               (* callee is an extern function, just add an edge (callee -> ret) *)
               (* Logger.result "callee_va %a of a library function"
                *   VA.pp callee_va; *)
               add_edge_a g callee_va ret_va;
             | f ->
               let ret_v = ret_nodes g f in
               (* Logger.result "ret node: %a" VA.pp_list ret_v; *)
               List.iter (fun v ->
                   add_edge_a g v ret_va) ret_v;
            );
          ) calls;
      ) (calls g)
  ;;

  let block_addr g va =
    let leader = H.fold (fun _ f acc ->
        let leader = H.fold (fun v insts ac ->
            if List.mem va insts then v :: ac
            else ac
          ) (F.blocks f) [] in
        match List.length leader with
        | 1 -> (List.hd leader) :: acc
        | _ -> acc
      ) g.funcs [] in
    if List.length leader > 0 then List.hd leader
    else VA.create 0
  ;;

  let get_func g f =
    let func = H.fold (fun k v acc ->
        match f with
        | Function.Name fs ->
          if String.equal fs (F.name v) then
            v :: acc
          else acc
        | Function.Address fa ->
          if VA.equal fa k then
            v :: acc
          else acc
      ) g.funcs [] in
    if List.length func = 1 then
      Some (List.hd func)
    else None
  ;;
end

module Read = struct
  let relabel_block g leader =
    match C.V.symb leader with
    | None -> ()
    | Some b ->
      let leader_a = C.V.addr leader in
      let func = S.func b in
      let symb' = S.create leader_a func in
      let rec aux = function
        | [] -> ()
        | h :: tl ->
          let recurse () =
            G.add_symb g (C.V.addr h) symb';
            aux tl;
            aux (G.succ g h) in
          match C.V.symb h with
          | None -> recurse ()
          | Some b' -> if S.equal b b' then recurse ()
      in aux [leader]
  ;;

  let func f ~line =
    Logger.debug "Parsing function %s" line;
    let l = IU.strip_enclosing_chars line in
    match String.split_on_char ';' l with
    | addr :: name :: _ ->
      let vaddr = IU.to_vaddr addr in
      (* update function's eps and name *)
      F.set_eps f (VA.Set.of_list [vaddr]);
      F.set_name f name;
    | _ ->
      let msg =
        Printf.sprintf
          "[parse_function] Error parsing line %s. \
           Expected a line with addr :: function name"
          line in
      failwith msg
  ;;

  let block g f ~simple ~line =
    Logger.debug "Parsing block %s" line;
    let elts = String.split_on_char ';' @@ IU.strip_enclosing_chars line in
    match elts with
    | addr :: insts :: bb_succs :: [calls_str] ->
      let va = IU.to_vaddr addr in
      let inst_addrs =
        IU.strip_enclosing_chars insts
        |> String.split_on_char ','
        |> List.map IU.to_vaddr in
      (* update function's basic blocks *)
      F.set_blocks f va inst_addrs;
      let list_insts = IU.read_list insts in
      (* add edges between instructions of a basic block to G *)
      (if not simple then begin
          let rec add_sequence = function
            | ia1 :: (ia2 :: _ as l) ->
              let va1 = IU.to_vaddr ia1 and va2 = IU.to_vaddr ia2 in
              F.set_edges f ((va1, va2) :: (F.edges f));
              G.add_edge_a g va1 va2;
              add_sequence l
            | _ -> () in
          add_sequence list_insts;
        end
      );
      let last_inst = List_utils.last list_insts |> IU.to_vaddr in
      (* adding block successors *)
      let is_empty_list = (=) "()" in
      (if not @@ is_empty_list bb_succs then
         let succs = IU.read_list bb_succs in
         List.iter (fun succ_str ->
             let succ_va = IU.to_vaddr succ_str in
             (* add edges to F and G *)
             let add_edges va =
               F.set_edges f ((va, succ_va) :: (F.edges f));
               G.add_edge_a g va succ_va; in
             if not simple then add_edges last_inst
             else add_edges va
           ) succs;
         (if not @@ is_empty_list calls_str then begin
             let calls = IU.parse_calls calls_str in
             List.iter (fun (_caller_va, callee_va, ret_va) ->
                 (* update function's (caller-callee) addr *)
                 F.set_calls f va [(callee_va, ret_va)];
                 G.add_calls g ~caller:va
                   ~callee:callee_va ~return:ret_va;
                 (* TODO ICFG: process (caller, callee, return) *)
                 (* if icfg then begin
                  *   let add_cr_edges va =
                  *     List.iter (fun succ_str ->
                  *         let succ_va = IU.to_vaddr succ_str in
                  *         G.remove_edge_a g va succ_va;
                  *       ) succs;
                  *     G.add_edge_a g va callee_va;
                  *     (\* here we are suppose to relabel the edge
                  *        as return edge before adding it *\)
                  *     match G.mem_vertex_a g va with
                  *     | None -> ()
                  *     | Some v ->
                  *       List.iter (relabel_block g) (G.succ g v);
                  *   in
                  *   (\* add call and return edges to generate ICFG *\)
                  *   if not (VA.equal callee_va (VA.create 0)) then begin
                  *     if not simple then add_cr_edges last_inst
                  *     else add_cr_edges va
                  *   end
                  * end *)
               ) calls;
           end
         );
      );
    | _ ->
      let msg =
        Printf.sprintf
          "[parse_bblock] could not parse %s. \
           Expected a line with \
           addr :: inst list :: succ bblock list :: callee"
          line in
      failwith msg
  ;;

  let update_inst g vaddr mnemonic opcode bb_addr func_name =
    match G.mem_vertex_a g vaddr with
    | Some _ ->
      let mnemonic =
        let mnemonic_hint = IU.clean_mnemonic mnemonic in
        Mnemonic.unsupported ~mnemonic_hint () in
      let opcode = Binstream.of_nibbles opcode in
      let size = Binstream.length opcode |> Size.Byte.create in
      let bb_vaddr = IU.to_vaddr bb_addr in
      let fname = Function.name func_name in
      let symb = S.create bb_vaddr fname in
      let inst =
        Instruction.create vaddr size opcode mnemonic Dhunk.empty in
      G.add_inst g vaddr inst;
      G.add_symb g vaddr symb;
    | None ->
      Logger.warning "Vertex %@ %a should have been created" VA.pp vaddr;
      let v = G.V.of_addr vaddr in
      G.add_vertex g v;
  ;;

  let inst g f ~simple ~line =
    Logger.debug "Parsing instruction %s" line;
    let clean_l = IU.strip_enclosing_chars line in
    match String.split_on_char ';' clean_l with
    | addr :: mnemonic :: opcode :: bb_addr :: fname :: _ ->
      let vaddr = IU.to_vaddr addr in
      (match simple with
       | false ->
         (* update all instructions of G *)
         update_inst g vaddr mnemonic opcode bb_addr fname;
       | true ->
         match List.mem vaddr (F.all_leaders f) with
         | true ->
           (* only update basic blocks of G *)
           update_inst g vaddr mnemonic opcode bb_addr fname;
         | false -> ()
      );
    | _ ->
      let msg = Printf.sprintf "Error parsing instruction from %s" line in
      failwith msg
  ;;
end

let gname s =
  let str = Str.regexp "\\." in
  Str.global_replace str "_" s
;;

module P =
  Graph.Graphviz.Dot(
  struct
    include C
    let graph_attributes _t = [] ;;
    let default_vertex_attributes _ = [`Shape `Box; `Style `Rounded;] ;;
    let vertex_name v = string_of_int @@ VA.to_int (V.addr v) ;;

    let vertex_attributes v =
      let label =
        let b = Buffer.create 32 in
        let ppf = Format.formatter_of_buffer b in
        VA.pp ppf (V.addr v);
        (match V.inst v with
         | None -> ()
         | Some i ->
           Format.pp_print_space ppf ();
           let m =
             let open Mnemonic in
             match Instruction.mnemonic i with
             | Unsupported (Some m) -> supported m Format.pp_print_string
             | m -> m in
           Mnemonic.pp ppf m);
        Format.pp_print_flush ppf ();
        Buffer.contents b
      in
      [ `Label label ]
    ;;

    let get_subgraph v =
      match V.symb v with
      | None -> None
      | Some s ->
        let open Graph.Graphviz.DotAttributes in
        let sg_name =
          gname @@
          match S.func s with
          | Function.Name n -> n
          | Function.Address a ->
            Print_utils.string_from_pp VA.pp a
        in
        let sg_attributes =
          [`Style `Filled; `Label sg_name; `Style `Rounded;] in
        let sg_parent = None in
        Some { sg_name; sg_attributes; sg_parent; }
    ;;

    let default_edge_attributes _ = [] ;;
    let edge_attributes _ = [] ;;
  end
  )

let default_cfg_filename = "cfg.dot" ;;

let gen_dot_cfgs g =
  let out_dir = Filename.concat (Sys.getcwd ()) "cfgs" in
  try
     Unix.mkdir out_dir 0o777;
     (* TODO: nodes of cfg's function don't have inst and symb *)
     H.iter (fun _ f ->
         let cfg_f = F.build_cfg f in
         let cfg_fname = Filename.concat out_dir (F.name f) in
         let oc = Pervasives.open_out_bin cfg_fname in
         P.output_graph oc cfg_f;
         close_out oc;
       ) (G.funcs g);
     let oc = Pervasives.open_out_bin default_cfg_filename in
     P.output_graph oc (G.graph g);
     close_out oc;
   with _ -> Logger.warning "Cannot create cfgs directory";
;;

let is_function_header = String.equal "-------------------" ;;

let of_file ~simple ~ida_file =
  let ic = open_in ida_file in
  let functions = Stack.create () in
  let rec loop line_num g =
    match input_line ic with
    | line ->
       Logger.debug "Reading line %d" line_num;
       (if String.length line > 0 then
          if is_function_header line then begin
              Logger.debug ~level:4 "Push";
              Stack.push (F.empty ()) functions
            end
          else begin
              (* update the most recently added function *)
              let f = Stack.top functions in
              (match String.get line 0 with
               | '{' -> Read.func f ~line
               | '[' -> Read.block g f ~simple ~line
               | '(' -> Read.inst g f ~simple ~line
               | _ -> Logger.warning "Ignoring line %s" line
              );
            end);
       loop (succ line_num) g
    | exception End_of_file -> (close_in ic; g) in
  let g = loop 1 (G.create ()) in
  (* finally, add functions to G *)
  Stack.iter (G.add_function g) functions;
  g
;;

let do_cfg ~simple ~ida_file =
  let g = of_file ~simple ~ida_file in
  Logger.debug ~level:3 "%a" G.pp g;
  if Ida_options.IdaCfg.get () then begin
      G.iter_vertex (G.disassemble_vertex g) g;
      gen_dot_cfgs g;
    end
  ;
  g
;;
