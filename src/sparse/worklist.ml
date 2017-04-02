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
open Dug
open BasicDom
open AbsDom

module type G =
sig 
  type t
  type node = BasicDom.Node.t
  val nb_node : t -> int
  val fold_edges : (node -> node -> 'a -> 'a) -> t -> 'a -> 'a 
end

module type S = 
sig
  module DUGraph : G
  type t
  val init : DUGraph.t -> t
  val pick : t -> (BasicDom.Node.t * t) option
  val push : BasicDom.Node.t -> BasicDom.Node.t -> t -> t
  val push_set : BasicDom.Node.t -> BasicDom.Node.t BatSet.t -> t -> t
  val is_loopheader : BasicDom.Node.t -> t -> bool
end 

module NGraph = struct
  module NOrder = struct
    type t = int [@@deriving compare]
    let hash = Hashtbl.hash
    let equal = ( = )
  end

  module G = Graph.Imperative.Digraph.Concrete (NOrder)
  include G
  include Graph.Components.Make (G)
  let add_edge g s d = add_edge g s d; g
  let remove_edge g s d = remove_edge g s d; g
  let empty size = create ~size:size ()
end 

module Make (DUGraph : G) = struct
  module DUGraph = DUGraph
  module Workorder = struct
    type t = {
      order : (DUGraph.node, int * bool) BatMap.t;
      headorder: (DUGraph.node, int) BatMap.t;
      loopheads : DUGraph.node BatSet.t
    }

    let empty = {
      order = BatMap.empty;
      headorder = BatMap.empty;
      loopheads = BatSet.empty
    }

    let make : DUGraph.t -> NGraph.t * (int, DUGraph.node) BatMap.t
    = fun g ->
      let i = ref 0 in
      let new_i () = let v = !i in i := !i + 1; v in
      let create n i2n n2i =
        try (BatMap.find n n2i, i2n, n2i) with Not_found ->
          let i = new_i () in
          (i, BatMap.add i n i2n, BatMap.add n i n2i) in
      let add_edge src dst (ng, i2n, n2i) =
        let (i_src, i2n, n2i) = create src i2n n2i in
        let (i_dst, i2n, n2i) = create dst i2n n2i in
        (NGraph.add_edge ng i_src i_dst, i2n, n2i) in
      let (ng, i2n, _) =
        DUGraph.fold_edges add_edge g (NGraph.empty (DUGraph.nb_node g), BatMap.empty, BatMap.empty) in
      (ng, i2n)

    let projection : NGraph.vertex BatSet.t -> NGraph.t -> NGraph.t
    = fun scc ng ->
      let add_back_edge e newg =
        let succ = NGraph.succ ng e in
        let succ = List.filter (fun x -> BatSet.mem x scc) succ in
        list_fold (fun s newg -> NGraph.add_edge newg e s) succ newg in
      BatSet.fold add_back_edge scc (NGraph.empty (BatSet.cardinal scc))

    let loophead_of scc ng =
      let add_entry src dst acc =
        if not (BatSet.mem src scc) && BatSet.mem dst scc then BatSet.add dst acc
        else acc in
      let entries = NGraph.fold_edges add_entry ng BatSet.empty in
      let get_score n =
        let preds = NGraph.pred ng n in
        let preds = List.filter (fun n -> BatSet.mem n scc) preds in
        List.length preds in
      let compare_score n (candidate, score) =
        let score_n = get_score n in
        if score_n > score then (n, score_n) else (candidate, score) in
      fst (BatSet.fold compare_score entries (BatSet.choose scc, 0))
  
    let cut_backedges ng entry =
      let preds = NGraph.pred ng entry in
      let cut_edge pred ng = NGraph.remove_edge ng pred entry in
      list_fold cut_edge preds ng

    let rec get_order sccs ng (wo, lhs, ho) order =
      match sccs with
      | scc :: t ->
        if List.length scc > 1 then
          let headorder = order + 3 * (List.length scc) in
          let scc = list2set scc in
          let ng' = projection scc ng in
          let lh = loophead_of scc ng in
          let (lhs, ho) = (BatSet.add lh lhs, BatMap.add lh headorder ho) in
          let (wo, lhs, ho, _) = get_order t ng (wo, lhs, ho) (headorder + 1) in
          let ng' = cut_backedges ng' lh in
          let sccs' = List.rev (NGraph.scc_list ng') in
          get_order sccs' ng' (wo, lhs, ho) order
        else
          let n = List.hd scc in
          get_order t ng (BatMap.add n order wo, lhs, ho) (order + 1)
      | [] -> (wo, lhs, ho, order)

    let is_loopheader here info = BatSet.mem here info.loopheads
  
    let rec perform g =
      Profiler.start_event "Worklist.make";
      let (ng, i2n) = make g in
      Profiler.finish_event "Worklist.make";
      let sccs = List.rev (NGraph.scc_list ng) in
      Profiler.start_event "Worklist.get_order";
      let (wo, lhs, ho, _) =
        get_order sccs ng (BatMap.empty, BatSet.empty, BatMap.empty) 0 in
      Profiler.finish_event "Worklist.get_order";

      let add_rec_node src dst nodes =
        if Pervasives.compare src dst = 0 then BatSet.add src nodes else nodes 
      in
      Profiler.start_event "Worklist.rec_node";
      let lhs = NGraph.fold_edges add_rec_node ng lhs in
      Profiler.finish_event "Worklist.rec_node";
      let trans_map trans_k trans_v m =
      let add_1 k v = BatMap.add (trans_k k) (trans_v k v) in
      BatMap.foldi add_1 m BatMap.empty in
      let trans_set trans_v s =
        let add_1 v = BatSet.add (trans_v v) in
        BatSet.fold add_1 s BatSet.empty 
      in
      let trans_k k = BatMap.find k i2n in
  
      Profiler.start_event "Worklist.trans";
      let wo = trans_map trans_k (fun k v -> (v, BatSet.mem k lhs)) wo in
      let lhs = trans_set (fun v -> BatMap.find v i2n) lhs in
      let ho = trans_map trans_k (fun _ v -> v) ho in
      Profiler.finish_event "Worklist.trans";
      { order = wo; headorder = ho; loopheads = lhs }
  end

  module Ord = struct
    type t = workorder * DUGraph.node
    and workorder = int * bool
    let compare ((o1, _), v1) ((o2, _), v2) =
      let cmp_o = o1 - o2 in
      let cmp_v = Pervasives.compare v1 v2 in
      if cmp_o = 0 then cmp_v else cmp_o
  end
  
  module S = Set.Make (Ord)
  type t = {
    set : S.t;
    order : Workorder.t;
  }
  let compare_order succ idx order = 
    try 
      let id1 = BatMap.find idx order.Workorder.order in
      let id2 = BatMap.find succ order.Workorder.order in
      Ord.compare (id2,succ) (id1,idx) <= 0
    with Not_found -> false

  let queue is_inneredge order n wl =
  (* change order if,
     - the n node has a loophead order, and
     - an inneredge to the n node is updated
  *)
    let rec change_order n o is_inneredge =
      let is_loophead = snd o in
      if is_inneredge && is_loophead then
        try (BatMap.find n order.Workorder.headorder, is_loophead) with Not_found -> o
      else o in
    let o = BatMap.find n order.Workorder.order in 
    let new_o = change_order n o is_inneredge in
    { wl with set = S.add (new_o, n) wl.set }

  let push : Node.t -> Node.t -> t -> t
  = fun idx succ ws ->
    let bInnerLoop = compare_order succ idx ws.order in
    queue bInnerLoop ws.order succ ws

  let push_set : Node.t -> Node.t BatSet.t -> t -> t
  = fun idx succs ws ->
    BatSet.fold (fun succ works ->
      let bInnerLoop = compare_order succ idx works.order in
      queue bInnerLoop works.order succ works
    ) succs ws

  let init dug = { set = S.empty; order = Workorder.perform dug }

  let is_loopheader idx ws = Workorder.is_loopheader idx ws.order  

  let pick ws =
    try
      let (o, n) = S.min_elt ws.set in
      let ws = { ws with set = S.remove (o, n) ws.set } in
      Some (n, ws)
    with Not_found -> None
end
