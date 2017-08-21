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
open Vocab
open Global
open BasicDom
open ItvDom

(* ***************************** *
 * Flow-insensitive pre-analysis *
 * ***************************** *)
let onestep_transfer : Node.t list -> Mem.t * Global.t -> Mem.t * Global.t
=fun nodes (mem,global) ->
  list_fold (fun node (mem,global) ->
    ItvSem.run AbsSem.Weak ItvSem.Spec.empty node (mem,global)
  ) nodes (mem,global)

let rec fixpt : Node.t list -> int -> Mem.t * Global.t -> Mem.t * Global.t
=fun nodes k (mem,global) ->
  my_prerr_string ("\riteration : " ^ string_of_int k);
  flush stderr;
  let (mem',global') = onestep_transfer nodes (mem,global) in
  let mem' = Mem.widen mem mem' in
    if Mem.le mem' mem && Dump.le global'.dump global.dump
    then (my_prerr_newline (); (mem',global'))
    else fixpt nodes (k+1) (mem',global')

let callees_of : InterCfg.t -> InterCfg.Node.t -> Mem.t -> PowProc.t
= fun icfg node mem ->
  let pid = InterCfg.Node.get_pid node in
  let c = InterCfg.cmdof icfg node in
  match c with
  | IntraCfg.Cmd.Ccall (_, e, _, _) ->
    Val.pow_proc_of_val (ItvSem.eval pid e mem)
  | _ -> PowProc.bot

let draw_call_edges : InterCfg.Node.t list -> Mem.t -> Global.t -> Global.t
= fun nodes mem global ->
  let icfg =
    List.fold_left (fun icfg node ->
        if InterCfg.is_callnode node icfg then
          let callees = callees_of icfg node mem in
          PowProc.fold (InterCfg.add_call_edge node) callees icfg
        else icfg) global.icfg nodes
  in
  { global with icfg = icfg }

let draw_callgraph : Node.t list -> Mem.t -> Global.t -> Global.t
=fun nodes mem global ->
  let callgraph = List.fold_left (fun callgraph node ->
      let callees = callees_of global.icfg node mem in
      PowProc.fold (fun callee callgraph ->
        CallGraph.add_edge (InterCfg.Node.get_pid node) callee callgraph) callees callgraph)
    global.callgraph nodes
    |> CallGraph.compute_trans_calls
  in
  { global with callgraph = callgraph }

let perform : Global.t -> Global.t
= fun global ->
  let nodes = InterCfg.nodesof global.icfg in
  let (mem, global) = fixpt nodes 1 (Mem.bot,global) in
  my_prerr_endline ("mem size : " ^ i2s (Mem.cardinal mem));
  { global with mem = mem }
  |> draw_call_edges nodes mem
  |> draw_callgraph nodes mem
  |> Global.remove_unreachable_functions
