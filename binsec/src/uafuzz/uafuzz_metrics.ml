open Uafuzz_options

module VA = Virtual_address
module H = VA.Htbl
module ICG = Ida_cg
module F = Ida_cfg.Function
module IC = Ida_cfg.C
module IF = Ida_cfg.F
module IG = Ida_cfg.G
module IU = Ida_utils
module UT = Uafuzz_targets
module UO = Uafuzz_options
module UU = Uafuzz_utils

module Distance = struct
  let aflgo_const = 10.0 ;; (* AFLGo's function-level distance const *)
  let he_const = 2 ;; (* default const value in Hawkeye *)
  let uaf_const = 0.25 ;; (* UAF distance const *)

  let cnt_n = ref 0 ;;
  let cnt_b = ref 0 ;;

  (* call site occurrences & nb of blocks containing callee *)
  let aaf_metrics g caller callee =
    match IG.get_func g caller with
    | Some f_caller ->
      let call_intrs_in_caller = IF.calls f_caller in
      let (nb_calls, blocks) =
        H.fold (fun va calls_in_caller (nb_calls_acc, bbs_acc) ->
            match IG.get_func g callee with
            | Some f_callee ->
              let ep_callee = IF.one_ep f_callee in
              let nb_calls_callee =
                List.length (
                  List.filter (fun (c, _r) ->
                      VA.equal c ep_callee) calls_in_caller) in
              let blocks =
                if nb_calls_callee > 0 then VA.Set.add va VA.Set.empty
                else VA.Set.empty in
              (nb_calls_acc + nb_calls_callee, (VA.Set.union bbs_acc blocks))
            | None -> (nb_calls_acc, bbs_acc)
          ) call_intrs_in_caller (0, VA.Set.empty)
      in (nb_calls, VA.Set.cardinal blocks)
    | None -> (0, 0)
  ;;

  (* augmented adjacent-function distance in Hawkeye *)
  let aaf_distance g caller callee =
    let caller_fname = ICG.Node.func (ICG.V.addr caller) in
    let callee_fname = ICG.Node.func (ICG.V.addr callee) in
    let (occur, nb_blocks) = aaf_metrics g caller_fname callee_fname in
    if occur > 0 && nb_blocks > 0 then begin
      (if occur > 1 then cnt_n := !cnt_n + 1;);
      (if nb_blocks > 1 then cnt_b := !cnt_b + 1;);
      (* call site occurrences phi_CN *)
      let phi_CN = (float_of_int (he_const * occur + 1))
                   /. (float_of_int (he_const * occur)) in
      (* nb of blocks phi_CB *)
      let phi_CB = (float_of_int (he_const * nb_blocks + 1))
                   /. (float_of_int (he_const * nb_blocks)) in
      let aaf_dist = phi_CN *. phi_CB in
      Logger.debug "%a -> %a: occur = %d, phi_CN = %f, \
                    nb_blocks = %d, phi_CB = %f, aaf_dist = %f"
        ICG.Node.pp (ICG.V.addr caller)
        ICG.Node.pp (ICG.V.addr callee)
        occur phi_CN nb_blocks phi_CB aaf_dist;
      aaf_dist
    end
    else 0.0
  ;;

  (* get a list of favored UAF edges in CG *)
  let get_uaf_edges cg uaf_target =
    let f_targets =
      List.map (fun n -> UT.Node.func n) uaf_target in
    let f_alloc = List.hd f_targets in
    let f_free = List.nth f_targets 1 in
    let f_use = List.nth f_targets 2 in
    let r_alloc = UU.reachable_funcs cg f_alloc in
    let r_free = UU.reachable_funcs cg f_free in
    let r_use = UU.reachable_funcs cg f_use in
    List.iter (fun f ->
        Logger.debug "f_alloc: %a, reachable alloc funcs: %a"
          F.pp f_alloc ICG.Node.pp (ICG.V.addr f);
      ) r_alloc;
    List.iter (fun f ->
        Logger.debug "f_free: %a, reachable free funcs: %a"
          F.pp f_free ICG.Node.pp (ICG.V.addr f);
      ) r_free;
    List.iter (fun f ->
        Logger.debug "f_use: %a, reachable use funcs: %a"
          F.pp f_use ICG.Node.pp (ICG.V.addr f);
      ) r_use;
    Logger.result "reachable alloc length: %d, free length: %d, use length: %d"
      (List.length r_alloc) (List.length r_free) (List.length r_use);
    let r_afu = List.filter (fun f -> List.mem f r_free && List.mem f r_use) r_alloc in
    let r_af = List.filter (fun f -> List.mem f r_free) r_alloc in
    let r_fu = List.filter (fun f -> List.mem f r_use) r_free in
    Logger.result "r_afu length: %d, r_af length: %d, r_fu length: %d"
      (List.length r_afu) (List.length r_af) (List.length r_fu);
    let uaf_edges = ICG.fold_edges (fun fa fb acc ->
        if
          (* fb can call malloc and free and can reach target *)
          (List.mem fb r_afu) ||
          (* fa can call malloc && fb can call free and reach target *)
          (List.mem fa r_alloc) && (List.mem fb r_fu) ||
          (* fa can call malloc and free && fb can reach target *)
          (List.mem fa r_af) && (List.mem fb r_use) ||
          (* fa can call alloc && fb can call free *)
          (List.mem fa r_alloc) && (List.mem fb r_free) ||
          (* fa can call free && fb can reach target *)
          (List.mem fa r_free) && (List.mem fb r_use) ||
          (* fa can call free && fb can call free *)
          (List.mem fa r_free) && (List.mem fb r_free)
        then begin
          let edge = ICG.Edge.create (ICG.V.addr fa) (ICG.V.addr fb) in
          edge :: acc end
        else acc) cg [] in
    List.iter (fun e ->
        Logger.debug "uaf edges: %a" ICG.Edge.pp e;
      ) uaf_edges;
    uaf_edges
  ;;

  let weights = Hashtbl.create 5000 ;;

  (* update weight of edges of CG *)
  let update_cg_weights cg g ~typ targets =
    match typ with
    | UO.HawkeyeB | UO.UAFuzz ->
      let uaf_edges = get_uaf_edges cg targets in
      (* Logger.result "uaf_edges length: %d" (List.length uaf_edges); *)
      ICG.iter_edges_e (fun e ->
          let caller = ICG.E.src e in
          let callee = ICG.E.dst e in
          let edge = ICG.Edge.create
              (ICG.V.addr caller) (ICG.V.addr callee) in
          let edge_w =
            let aaf_dist = aaf_distance g caller callee in
            if typ = UO.UAFuzz && List.mem edge uaf_edges then
              aaf_dist *. uaf_const
            else aaf_dist in
          Hashtbl.add weights e edge_w;
        ) cg;
      (* Logger.result "cnt_n: %d, cnt_b: %d" !cnt_n !cnt_b; *)
    | _ -> (* AFLGoB *)
      ICG.iter_edges_e (fun e -> Hashtbl.add weights e 1.0) cg;
  ;;

  module W = struct
    type edge = ICG.E.t
    type t = float
    let weight x = Hashtbl.find weights x
    let zero = 0.0
    let add = (+.)
    let compare = compare
  end

  module Dij = Graph.Path.Dijkstra(ICG)(W)

  let shortest_len cg src dst =
    let (_, w) = Dij.shortest_path cg src dst in
    w
  ;;

  (* compute function level distance df(n, Tf) on CG *)
  let flevel_distance cg g targets =
    let cg_fname_dists = Hashtbl.create 5000 in
    let cg_addr_dists = Hashtbl.create 5000 in
    let ftargets =
      ICG.fold_vertex (fun v acc ->
          let f = ICG.Node.func (ICG.V.addr v) in
          if List.mem f (UT.funcs targets) then v :: acc
          else acc
        ) cg [] in
    ICG.iter_vertex (fun f ->
        let dist_cg = ref (-1.0) in
        let d = ref 0.0 in
        let i = ref 0.0 in
        let fname = ICG.Node.func (ICG.V.addr f) in
        (* harmonic distance from f to "reachable" target functions *)
        List.iter (fun ft ->
            if ICG.is_reachable cg f ft then
              let shortest = shortest_len cg f ft in
              Logger.debug "shortest len (%a, %a): %f"
                ICG.Node.pp_short (ICG.V.addr f)
                ICG.Node.pp_short (ICG.V.addr ft)
                shortest;
              d := !d +. 1.0 /. (1.0 +. shortest);
              i := !i +. 1.0;
          ) ftargets;
        (* Logger.result "d: %f, i: %f, dist: %f" !d !i (!i /. !d); *)
        if (!d != 0.0) &&
           ((!dist_cg == -1.0) || (!dist_cg > (!i /. !d))) then begin
          dist_cg := !i /. !d;
          (match IG.get_func g fname with
            | Some f ->
              Logger.debug "%a, %a, %f" VA.pp (IF.one_ep f) F.pp fname !dist_cg;
              Hashtbl.add cg_addr_dists (IF.one_ep f) dist_cg;
            | None -> ()
          );
          Hashtbl.add cg_fname_dists fname dist_cg;
        end
      ) cg;
    Hashtbl.iter (fun f cg_d ->
        Logger.debug "function-level distance %a: %f" F.pp f !cg_d;
      ) cg_fname_dists;
    cg_fname_dists, cg_addr_dists
  ;;

  (* compute basic block level distance db(m, Tb) on CFG *)
  let bblevel_distance cg g targets =
    let start = Sys.time () in
    let (cg_dists, _) = flevel_distance cg g targets in
    let trans_dists = Hashtbl.create 5000 in
    let cfg_dists = Hashtbl.create 50000 in
    (* distance from bb transition to targets based on flevel distance *)
    H.iter (fun caller_va calls ->
        List.iter (fun (callee_va, _ret_va) ->
            match IG.fname_of_va g callee_va with
            | None -> ()
            | Some callee_fname ->
              match ICG.node_from_fname cg callee_fname with
              | None -> ()
              | Some _ ->
                Logger.debug "%a, (%a:%a)"
                  VA.pp caller_va VA.pp callee_va F.pp callee_fname;
                match Hashtbl.find cg_dists callee_fname with
                | exception Not_found -> ()
                | shortest ->
                  Hashtbl.add trans_dists caller_va !shortest;
          ) calls;
      ) (IG.calls g);
    Hashtbl.iter (fun va call_d ->
        Logger.debug "transition distance %a: %f" VA.pp va call_d;
      ) trans_dists;
    (* distance from bb to "reachable" bb transition *)
    Hashtbl.iter (fun fname _ ->
        match IG.get_func g fname with
        | Some f ->
          let fcfg = IF.build_cfg f in
          IC.iter_vertex (fun bbv ->
              let dist_cfg = ref (-1.0) in
              let d = ref 0.0 in
              let i = ref 0.0 in
              let bbva = IC.V.addr bbv in
              Hashtbl.iter (fun caller_va cfg_d ->
                  if IC.is_reachable_va fcfg bbva caller_va then begin
                    (match IC.shortest_len fcfg bbva caller_va with
                     | Some shortest ->
                       d := !d +. 1.0 /. (1.0 +. aflgo_const *. cfg_d +. shortest);
                      i := !i +. 1.0;
                    | None -> ()
                    );
                  end
                ) trans_dists;
              if (!d != 0.0) &&
                ((!dist_cfg == -1.0) || (!dist_cfg > (!i /. !d))) then begin
                dist_cfg := !i /. !d;
                Hashtbl.add cfg_dists bbva dist_cfg;
              end
            ) fcfg;
        | None -> ()
        (* Logger.result "d: %f, i: %f, dist: %f" !d !i (!i /. !d); *)
      ) cg_dists;
    let dist_time = Sys.time () in
    Logger.debug "distance time: %f (s)" (dist_time -. start);
    (* Hashtbl.iter (fun bb cfg_d ->
     *     Logger.result "basic block level distance %a: %f" VA.pp bb !cfg_d;
     *   ) cfg_dists; *)
    cfg_dists
  ;;

  let to_fdist_file ~file cg_dists =
    let oc = open_out file in
    Unix.putenv "FN_DISTANCE_ENV_VAR" file;
    Logger.result "Function-level distance file: %s" file;
    Hashtbl.iter (fun va len ->
        Printf.fprintf oc "0x%x,%f\n" (VA.to_int va) !len;
      ) cg_dists;
    close_out oc;
  ;;

  let to_bbdist_file ~file cfg_dists =
    let oc = open_out file in
    Unix.putenv "BB_DISTANCE_ENV_VAR" file;
    Logger.result "BB-level distance file: %s" file;
    Hashtbl.iter (fun va len ->
        Printf.fprintf oc "0x%x,%f\n" (VA.to_int va) !len;
      ) cfg_dists;
    close_out oc;
  ;;
end

module Cutedge = struct
  module T = struct
    type t =
      | Cut
      | Non_cut

    let to_string = function
      | Cut -> "C"
      | Non_cut -> "N"

    let pp ppf t = Format.fprintf ppf "%s" (to_string t) ;;
  end

  module Edge = struct
    type t = {
      typ : T.t;
      src : VA.t;
      dst : VA.t;
    }

    let typ t = t.typ ;;
    let src t = t.src ;;
    let dst t = t.dst ;;

    let create t s d =
      { typ = t; src = s; dst = d }
    ;;

    let pp ppf t =
      Format.fprintf ppf "[%a] %a -> %a"
        T.pp t.typ VA.pp t.src VA.pp t.dst
    ;;

    let pp_list ppf t =
      Format.fprintf ppf "%a"
        (Print_utils.pp_list ~sep:"\n" pp) t
    ;;
  end

  let pair_processed = ref [] ;;
  let dec_nodes_processed = ref [] ;;
  let cut_edges f src dst =
    let ep = List.hd (VA.Set.elements (IF.eps f)) in
    Logger.debug "ep: %a, src: %a, dst: %a"
      VA.pp ep VA.pp src VA.pp dst;
    let f_cfg = IF.build_cfg f in
    let dec_nodes = IC.decision_nodes f_cfg in
    let cuts = List.filter (fun va ->
        IC.is_reachable_va f_cfg va dst &&
        (IC.is_reachable_va f_cfg va src ||
         IC.is_reachable_va f_cfg src va)
      ) dec_nodes in
    Logger.debug "decs: %a; cuts: %a" VA.pp_list dec_nodes VA.pp_list cuts;
    List.fold_left (fun acc va ->
        if not (List.mem va !dec_nodes_processed) then begin
          match IC.mem_vertex_a f_cfg va with
          | Some v ->
            dec_nodes_processed := [va] @ !dec_nodes_processed;
            let succs = IC.succ f_cfg v in
            let reachable_succs = List.filter (fun sv ->
                let sva = IC.V.addr sv in
                (* same function: ep ->* src ->* sva ->* dst *)
                (IC.exist_path f_cfg va src dst && IC.exist_path f_cfg sva src dst) ||
                (IC.exist_path f_cfg src va dst && IC.exist_path f_cfg src sva dst)
              ) succs in
            let non_reachable_succs = List.filter (fun sv ->
                not (List.mem sv reachable_succs)
              ) succs in
            List.iter (fun v ->
                Logger.debug "r_succs: %a" VA.pp (IC.V.addr v);
              ) reachable_succs;
            let c_edges = List.map (fun rsv ->
                Edge.create T.Cut va (IC.V.addr rsv)
              ) reachable_succs in
            let nc_edges = List.map (fun rsv ->
                Edge.create T.Non_cut va (IC.V.addr rsv)
              ) non_reachable_succs in
            Logger.debug "cut_edges: %a, noncut_edges: %a"
              Edge.pp_list c_edges Edge.pp_list nc_edges;
            IU.remove_from_right (c_edges @ nc_edges @ acc)
          | None -> acc
        end
        else acc
      ) [] cuts
  ;;

  let accumulate_cut_edges g pair_targets =
    List.fold_left (fun e_acc t ->
        let src = UT.Pair.src t in
        let dst = UT.Pair.dst t in
        let bb_addr_src = IG.block_addr g (UT.Node.addr src) in
        let bb_addr_dst = IG.block_addr g (UT.Node.addr dst) in
        let fname_src = UT.Node.func src in
        let fname_dst = UT.Node.func dst in
        match IG.get_func g fname_src with
        | Some f_src ->
          (
            match IG.get_func g fname_dst with
            | Some f_dst ->
              (* different functions: (f_src: ep -> bb_src) && (f_dst: ep -> bb_dst) *)
              (if not (F.same fname_src fname_dst) then begin
                  Logger.debug "====================\ndifferent functions: %a,%a; %a,%a"
                    F.pp fname_src VA.pp bb_addr_src
                    F.pp fname_dst VA.pp bb_addr_dst;
                  let ep_src = List.hd (VA.Set.elements (IF.eps f_src)) in
                  let ep_dst = List.hd (VA.Set.elements (IF.eps f_dst)) in
                  let cut_edges_src =
                    if not (List.mem src !pair_processed) then begin
                      pair_processed := [src] @ !pair_processed;
                      cut_edges f_src ep_src bb_addr_src
                    end
                    else [] in
                  Logger.debug "##########\n";
                  let cut_edges_dst = cut_edges f_dst ep_dst bb_addr_dst in
                  pair_processed := [dst] @ !pair_processed;
                  IU.remove_from_right (cut_edges_src @ cut_edges_dst @ e_acc)
                end
               else begin
                 (* same functions: (f_src: bb_src -> bb_dst) *)
                 Logger.debug "====================\nsame functions: %a, %a -> %a"
                   F.pp fname_dst VA.pp bb_addr_src VA.pp bb_addr_dst;
                 let cut_edges = cut_edges f_dst bb_addr_src bb_addr_dst in
                 pair_processed := [src] @ [dst] @ !pair_processed;
                 IU.remove_from_right (cut_edges @ e_acc)
               end
              );
            | None -> e_acc
          );
        | None -> e_acc
      ) [] pair_targets
  ;;

  let to_file ~file cut_edges =
    let oc = open_out file in
    Logger.result "cut edges file: %s" file;
    Unix.putenv "CUTEDGES_ENV_VAR" file;
    Logger.result "%a" Edge.pp_list cut_edges;
    List.iter (fun e ->
        Printf.fprintf oc "%s,0x%x,0x%x\n"
          (T.to_string (Edge.typ e)) (VA.to_int (Edge.src e))
          (VA.to_int (Edge.dst e));
      ) (IU.remove_from_right cut_edges);
    close_out oc;
  ;;
end

module Trace_closure = struct
  (* Target function trace closure: set of predecessors that can
   * statically lead to the target functions *)
  let targets_trace_closure cg targets =
    let ttc_nodes =
      List.fold_left (fun acc_target ftarget ->
          let nodes_t =
            match ICG.node_from_fname cg ftarget with
            | None -> []
            | Some node_target ->
              match ICG.node_from_fname cg (F.Name "main") with
              | None -> []
              | Some node_main ->
                ICG.fold_vertex (fun t acc_t ->
                    if (ICG.is_reachable cg node_main t) &&
                       (ICG.is_reachable cg t node_target) then
                      (ICG.V.addr t) :: acc_t
                    else acc_t
                  ) cg []
          in
          nodes_t @ acc_target
        ) [] targets
    in IU.remove_from_right ttc_nodes
  ;;

  let to_trace_closure_file ~file g nodes =
    let oc = open_out file in
    Unix.putenv "TRACE_CLOSURE_ENV_VAR" file;
    Logger.result "targets trace closure file: %s" file;
    List.iter (fun node ->
        Logger.result "func in trace closure: %a" ICG.Node.pp node;
        let func = ICG.Node.func node in
        (match IG.get_func g func with
         | Some f ->
           let faddr = IF.one_ep f in
           Logger.debug "%a, %a" VA.pp faddr F.pp func;
           Printf.fprintf oc "0x%x,%s\n" (VA.to_int faddr) (F.to_string func);
         | None -> ()
        );
      ) nodes;
    close_out oc;
  ;;
end
