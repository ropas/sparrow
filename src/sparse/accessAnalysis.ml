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
  module PowLoc : PowDom.CPO
  module Access : Access.S with type Loc.t = Loc.t and type PowLoc.t = PowLoc.t

  val perform : Global.t -> Access.PowLoc.t -> (BasicDom.Node.t -> Dom.t * Global.t -> Dom.t * Global.t) -> Dom.t -> Access.t
end

module Make(Sem : AccessSem.S)  =
struct
  module Access = Sem.Dom.Access
  module Loc = Sem.Dom.A
  module PowLoc = Sem.Dom.PowA
  module Dom = Sem.Dom
  module LocMap = Access.LocMap
  let init_access : Global.t -> PowLoc.t -> Dom.t -> (Node.t -> Dom.t * Global.t -> Dom.t * Global.t) -> Access.t
  =fun global locset mem f ->
    let nodes = InterCfg.nodesof global.icfg in
    let initial =
        list_fold (fun node ->
          Access.add_node node (Sem.accessof ~locset global node f mem)
       )  nodes Access.empty
    in
    let abslocs = Access.fold (fun _ access acc ->
        PowLoc.union_small_big (Access.Info.useof access) acc
        |> PowLoc.union_small_big (Access.Info.defof access)) initial PowLoc.empty
    in
    Access.add_total_abslocs abslocs initial

  let init_access_proc : Access.t -> Access.t
  =fun access ->
    let add_access_node node access m =
      Access.add_proc (Node.get_pid node) access m in
    Access.fold add_access_node access access

  let init_access_proc_reach : InterCfg.pid list -> CallGraph.t -> Access.t -> Access.t
  = fun pids callgraph access ->
    list_fold (fun pid ->
        let trans = CallGraph.trans_callees pid callgraph in
        let info =
          Access.Info.empty
          |> PowProc.fold (fun callee ->
               Access.Info.union (Access.find_proc callee access)) trans
          |> Access.Info.union (Access.find_proc pid access)
        in
        Access.add_proc_reach pid info) pids access

  let init_access_proc_reach_wo_local : InterCfg.pid list -> CallGraph.t
    -> Access.t -> Access.t
  = fun pids callgraph access ->
    list_fold (fun pid ->
        let trans = CallGraph.trans_callees pid callgraph |> PowProc.add pid in
        Access.find_proc_reach pid access
        |> PowProc.fold (fun callee ->
              let local = Access.find_proc_local callee access in
              Access.Info.filter_out local) trans
        |> Access.add_proc_reach_wo_local pid) pids access

  let init_access_proc_local : Access.t -> Access.t
  = fun access ->
    let update_loc2proc_1 pid loc (loc2proc, nonlocals) =
      if LocMap.mem loc loc2proc then
        let loc2proc = LocMap.remove loc loc2proc in
        let nonlocals = PowLoc.add loc nonlocals in
        (loc2proc, nonlocals)
      else
        let loc2proc = LocMap.add loc pid loc2proc in
        (loc2proc, nonlocals) in
    let update_loc2proc pid acc_of_pid (loc2proc, nonlocals) =
      let locs = PowLoc.diff (Access.Info.accessof acc_of_pid) nonlocals in
      PowLoc.fold (update_loc2proc_1 pid) locs (loc2proc, nonlocals) in
    let (loc2proc, _) =
      Access.fold_proc update_loc2proc access (LocMap.empty, PowLoc.empty) in
    let add_loc_pid loc pid = Access.add_proc_local pid loc in
    LocMap.fold add_loc_pid loc2proc access

  let init_access_program_local : Access.t -> Access.t
  =fun access ->
    let local = Access.fold_proc_local
        (fun _ local acc -> PowLoc.union acc local) access PowLoc.empty in
    Access.add_program_local local access

  let init_defs_of : Access.t -> Access.t
  =fun access ->
    Access.fold (fun node info access ->
      PowLoc.fold (fun loc access ->
        let old_nodes = Access.find_def_nodes loc access in
        Access.add_def_nodes loc (PowNode.add node old_nodes) access
      ) (Access.Info.defof info) access
    ) access access

  let init_uses_of : Access.t -> Access.t
  =fun access ->
    Access.fold (fun node info access ->
      PowLoc.fold (fun loc access ->
        let old_nodes = Access.find_use_nodes loc access in
        Access.add_use_nodes loc (PowNode.add node old_nodes) access
      ) (Access.Info.useof info) access
    ) access access

  let perform : Global.t -> Access.PowLoc.t -> (Node.t -> Dom.t * Global.t -> Dom.t * Global.t) -> Dom.t -> Access.t
  =fun global locset sem mem ->
    let pids = InterCfg.pidsof global.icfg in
    init_access global locset mem sem
    |> init_access_proc
    |> init_access_proc_reach pids global.callgraph
    |> init_access_proc_local
    |> init_access_proc_reach_wo_local pids global.callgraph
    |> init_access_program_local
    |> init_defs_of
    |> init_uses_of
end
