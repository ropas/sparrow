(***********************************************************************)
(*                                                                     *)
(* Copyright (c) 2007-present.                                         *)
(* Programming Research Laboratory (ROPAS), Seoul National University. *)
(* All rights reserved.                                                *)
(*                                                                     *)
(* This software is distributed under the term of the BSD license.     *)
(* See the LICENSE file for details.                                   *)
(*                                                                     *)
(***********************************************************************)
(* Access Pre-Analysis Framework *)
open Vocab
open Global
open BasicDom
open AbsSem
open Access
open ItvDom

module type S = 
sig
  module Dom : InstrumentedMem.S
  module Loc : AbsDom.SET
  module PowLoc : PowDom.S
  module LocMap : BatMap.S with type key = Loc.t
  module Access : Access.S with type Loc.t = Loc.t and type PowLoc.t = PowLoc.t

  type t
  val empty : t
  val get_total_abslocs : t -> Access.PowLoc.t
  val get_access : t -> Node.t -> Access.t
  val get_access_proc : t -> Proc.t -> Access.t
  val get_access_reach : t -> Proc.t -> Access.t
  val get_access_reach_wo_local : t -> BasicDom.Proc.t -> Access.t
  val get_access_local : t -> Proc.t -> Access.PowLoc.t
  val get_access_local_program : t -> Access.PowLoc.t
  val get_defs_of : t -> PowNode.t LocMap.t
  val get_uses_of : t -> PowNode.t LocMap.t
  val get_single_defs : PowNode.t LocMap.t -> Access.PowLoc.t
  val restrict_access   : t -> Access.PowLoc.t -> t
  val perform : Global.t -> Access.PowLoc.t -> (BasicDom.Node.t -> Dom.t * Global.t -> Dom.t * Global.t) -> Dom.t -> t
end

module Make(Sem : AccessSem.S)  =
struct
  module Access = Sem.Dom.Access
  module Loc = Sem.Dom.A
  module PowLoc = Sem.Dom.PowA
  module Dom = Sem.Dom
  module LocMap = BatMap.Make(Loc)
  module NodeMap = BatMap.Make(Node)
  module ProcMap = BatMap.Make(Proc)
  type t = {
    total_abslocs : PowLoc.t;
    access : Access.t NodeMap.t;
    access_proc : Access.t ProcMap.t;
    access_reach : Access.t ProcMap.t;
    access_reach_wo_local : Access.t ProcMap.t;
    access_local_proc : PowLoc.t ProcMap.t;
    access_local_program : PowLoc.t;
    defs_of : PowNode.t LocMap.t;
    uses_of : PowNode.t LocMap.t
  }

  let empty = {
    total_abslocs = PowLoc.empty;
    access = NodeMap.empty;
    access_proc = ProcMap.empty;
    access_reach = ProcMap.empty;
    access_reach_wo_local = ProcMap.empty;
    access_local_proc = ProcMap.empty;
    access_local_program = PowLoc.empty;
    defs_of = LocMap.empty;
    uses_of = LocMap.empty
  }

  let get_total_abslocs : t -> PowLoc.t
  =fun i -> i.total_abslocs

  let get_access : t -> Node.t -> Access.t
  =fun i n -> try NodeMap.find n i.access with _ -> Access.empty

  let get_access_proc : t -> Proc.t -> Access.t
  =fun i pid -> try ProcMap.find pid i.access_proc with _ -> Access.empty

  (* abstract locations exclusively accessed in the given function *)
  let get_access_local : t -> Proc.t -> PowLoc.t
  =fun i pid -> try ProcMap.find pid i.access_local_proc with _ -> PowLoc.empty

  let get_access_local_program : t -> PowLoc.t
  =fun i -> i.access_local_program

  let get_access_reach : t -> Proc.t -> Access.t
  =fun i pid -> try ProcMap.find pid i.access_reach with _ -> Access.empty

  let get_access_reach_wo_local : t -> Proc.t -> Access.t
  =fun i pid -> try ProcMap.find pid i.access_reach_wo_local with _ -> Access.empty

  let get_defs_of : t -> PowNode.t LocMap.t
  =fun t -> t.defs_of

  let get_uses_of : t -> PowNode.t LocMap.t
  =fun t -> t.uses_of

  let restrict : PowLoc.t -> Access.t NodeMap.t -> Access.t NodeMap.t
  =fun abslocs map ->
    NodeMap.map (fun access -> Access.restrict abslocs access) map

  let init_access : Global.t -> PowLoc.t -> Dom.t -> (Node.t -> Dom.t * Global.t -> Dom.t * Global.t) -> PowLoc.t * Access.t NodeMap.t
  =fun global locset mem f -> 
    let nodes = InterCfg.nodesof global.icfg in
    let map =
      list_fold (fun node ->
        NodeMap.add node (Sem.accessof ~locset global node f mem)
      ) nodes NodeMap.empty in
    let abslocs = NodeMap.fold (fun _ access acc ->
      let acc = PowLoc.union_small_big (Access.useof access) acc in
      PowLoc.union_small_big (Access.defof access) acc) map PowLoc.empty in
    (abslocs, restrict abslocs map)


  let filter_out_access : Access.t NodeMap.t -> Access.PowLoc.t -> Access.t NodeMap.t
  =fun access locs ->  
    NodeMap.fold (fun node access ->
      NodeMap.add node (Access.filter_out locs access) 
    ) access NodeMap.empty

  let restrict_access : t -> Access.PowLoc.t -> t
  =fun t locs ->  
    { t with access = 
        NodeMap.fold (fun node access ->
          NodeMap.add node (Access.restrict locs access) 
        ) t.access NodeMap.empty }

  let init_access_proc : Access.t NodeMap.t -> Access.t ProcMap.t
  =fun access ->
    let add_access_proc pid access m =
      ProcMap.modify_def Access.empty pid (Access.union access) m in
    let add_access_node node access m =
      add_access_proc (Node.get_pid node) access m in
    NodeMap.fold add_access_node access ProcMap.empty

  let init_access_reach : InterCfg.pid list -> CallGraph.t
    -> Access.t ProcMap.t -> Access.t ProcMap.t
  = fun pids callgraph access_proc ->
    list_fold (fun pid ->
        let trans = CallGraph.trans_callees pid callgraph in
        Access.empty 
        |> PowProc.fold (fun callee -> 
             Access.union (try ProcMap.find callee access_proc with _ -> Access.empty)) trans
        |> Access.union (try ProcMap.find pid access_proc with _ -> Access.empty)
        |> ProcMap.add pid) pids ProcMap.empty
 
  let init_access_reach_wo_local : InterCfg.pid list -> CallGraph.t
    -> Access.t ProcMap.t -> Access.PowLoc.t ProcMap.t
    -> Access.t ProcMap.t
  = fun pids callgraph access_reach access_local_proc ->
    list_fold (fun pid ->
        let trans = CallGraph.trans_callees pid callgraph |> PowProc.add pid in
        ProcMap.find pid access_reach
        |> PowProc.fold (fun callee ->
              let local = try ProcMap.find callee access_local_proc with _ -> PowLoc.empty in
              Access.filter_out local) trans
        |> ProcMap.add pid) pids ProcMap.empty

  let init_access_local_proc : Access.t ProcMap.t
      -> PowLoc.t ProcMap.t
  = fun access_proc ->
    let update_loc2proc_1 pid loc (loc2proc, nonlocals) =
      if LocMap.mem loc loc2proc then
        let loc2proc = LocMap.remove loc loc2proc in
        let nonlocals = PowLoc.add loc nonlocals in
        (loc2proc, nonlocals)
      else
        let loc2proc = LocMap.add loc pid loc2proc in
        (loc2proc, nonlocals) in
    let update_loc2proc pid acc_of_pid (loc2proc, nonlocals) =
      let locs = PowLoc.diff (Access.accessof acc_of_pid) nonlocals in
      PowLoc.fold (update_loc2proc_1 pid) locs (loc2proc, nonlocals) in
    let (loc2proc, _) =
      ProcMap.fold update_loc2proc access_proc (LocMap.empty, PowLoc.empty) in
    let proc2locs = ProcMap.map (fun _ -> PowLoc.empty) access_proc in
    let add_loc_pid loc pid = ProcMap.modify pid (PowLoc.add loc) in
    LocMap.fold add_loc_pid loc2proc proc2locs

  let init_access_local_program : PowLoc.t ProcMap.t -> PowLoc.t
  =fun access_local_proc ->
    ProcMap.fold (fun proc access -> PowLoc.union access) access_local_proc PowLoc.empty

  let init_defs_of : Access.t NodeMap.t -> PowNode.t LocMap.t
  =fun access_map ->
    NodeMap.fold (fun node access defs_of ->
      PowLoc.fold (fun loc defs_of ->
        let old_nodes = try LocMap.find loc defs_of with _ -> PowNode.empty in
        LocMap.add loc (PowNode.add node old_nodes) defs_of
      ) (Access.defof access) defs_of
    ) access_map LocMap.empty

  let init_uses_of : Access.t NodeMap.t -> PowNode.t LocMap.t
  =fun access_map ->
    NodeMap.fold (fun node access uses_of ->
      PowLoc.fold (fun loc uses_of ->
        let old_nodes = try LocMap.find loc uses_of with _ -> PowNode.empty in
        LocMap.add loc (PowNode.add node old_nodes) uses_of
      ) (Access.useof access) uses_of
    ) access_map LocMap.empty

  let get_single_defs : PowNode.t LocMap.t -> Access.PowLoc.t
  =fun defs_of -> 
    LocMap.fold (fun loc nodes -> 
      if PowNode.cardinal nodes = 1 then PowLoc.add loc else id
    ) defs_of PowLoc.empty

  let perform : Global.t -> Access.PowLoc.t -> (Node.t -> Dom.t * Global.t -> Dom.t * Global.t) -> Dom.t -> t
  =fun global locset sem mem ->
    let pids = InterCfg.pidsof global.icfg in
    let (total_abslocs, access) = init_access global locset mem sem in
    let access_proc = init_access_proc access in
    let access_reach = init_access_reach pids global.callgraph access_proc in
    let access_local_proc = init_access_local_proc access_proc in
    let access_reach_wo_local = init_access_reach_wo_local pids global.callgraph access_reach access_local_proc in
    let access_local_program = init_access_local_program access_local_proc in
    let defs_of = init_defs_of access in
    let uses_of = init_uses_of access in
    { total_abslocs; access; access_proc; access_reach; access_reach_wo_local; access_local_proc;
      access_local_program; defs_of; uses_of; }
end
