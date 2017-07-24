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
open AbsDom
open BasicDom

module type S = 
sig
  module Loc : SET
  module PowLoc : PowDom.CPO with type elt = Loc.t
  module Info :
  sig
    type t
    type kind
    val empty : t
    val def : kind
    val use : kind
    val all : kind
    val add : kind -> Loc.t -> t -> t
    val singleton : kind -> Loc.t -> t
    val mem : Loc.t -> t -> bool
    val add_set : kind -> PowLoc.t -> t -> t
    val from_set : kind -> PowLoc.t -> t
    val add_list : kind -> Loc.t list -> t -> t
    val union : t -> t -> t
    val diff : t -> t -> PowLoc.t
    val restrict : PowLoc.t -> t -> t
    val filter_out : PowLoc.t -> t -> t
    val accessof : t -> PowLoc.t
    val useof : t -> PowLoc.t
    val defof : t -> PowLoc.t
    val cardinal : t -> int
    val to_string_use : t -> string
    val to_string_def : t -> string
    val to_string : t -> string
    val print : t -> unit
    val print_use : t -> unit
    val print_def : t -> unit
  end

  module LocMap : BatMap.S with type key = Loc.t
  type t
  type info = Info.t
  val empty : t
  val add_node : BasicDom.Node.t -> Info.t -> t -> t
  val add_proc : BasicDom.Proc.t -> Info.t -> t -> t
  val add_proc_reach : BasicDom.Proc.t -> Info.t -> t -> t
  val add_proc_local : BasicDom.Proc.t -> Loc.t -> t -> t
  val add_proc_reach_wo_local : BasicDom.Proc.t -> Info.t -> t -> t
  val add_program_local : PowLoc.t -> t -> t
  val add_def_nodes : Loc.t -> BasicDom.PowNode.t -> t -> t
  val add_use_nodes : Loc.t -> BasicDom.PowNode.t -> t -> t
  val add_total_abslocs : PowLoc.t -> t -> t
  val fold : (BasicDom.Node.t -> Info.t -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_proc : (BasicDom.Proc.t -> Info.t -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_proc_local : (BasicDom.Proc.t -> PowLoc.t -> 'a -> 'a) -> t -> 'a -> 'a
  val total_abslocs : t -> PowLoc.t
  val find_node : BasicDom.Node.t -> t -> Info.t
  val find_proc : Proc.t -> t -> Info.t
  val find_proc_reach : Proc.t -> t -> Info.t
  val find_proc_reach_wo_local : BasicDom.Proc.t -> t -> Info.t
  val find_proc_local : BasicDom.Proc.t -> t -> PowLoc.t
  val find_program_local : t -> PowLoc.t
  val find_def_nodes : Loc.t -> t -> BasicDom.PowNode.t
  val find_use_nodes : Loc.t -> t -> BasicDom.PowNode.t
  val find_single_defs : t -> PowLoc.t
  val restrict_access : t -> PowLoc.t -> t
end

module Make(Dom: MapDom.CPO) = 
struct
  module PowLoc = Dom.PowA
  module Loc = Dom.A

  module Info = 
  struct
    type kind = DEF | USE | ALL
    type t = {
      def    : PowLoc.t;
      use    : PowLoc.t
    }
    let def = DEF
    let use = USE
    let all = ALL
    let empty = {
      def    = PowLoc.empty;
      use    = PowLoc.empty
    }

    let add : kind -> Loc.t -> t -> t
    = fun m a info ->
      match m with
      | USE -> { info with use = PowLoc.add a info.use }
      | DEF -> { info with def = PowLoc.add a info.def }
      | ALL -> { use = PowLoc.add a info.use;
                 def = PowLoc.add a info.def }

    let singleton : kind -> Loc.t -> t
    = fun m a -> add m a empty
   
    let mem : Loc.t -> t -> bool
    = fun l a ->
      (PowLoc.mem l a.def) || (PowLoc.mem l a.use)

    let remove : Loc.t -> t -> t
    = fun a info ->
      {
        use = PowLoc.remove a info.use;
        def = PowLoc.remove a info.def;
      } 

    let remove_set : PowLoc.t -> t -> t
    = fun addrs info ->
      { 
        use = PowLoc.diff info.use addrs;
        def = PowLoc.diff info.def addrs;
      }

    let add_set : kind -> PowLoc.t -> t -> t
    = fun m aset info ->
      match m with
      | USE -> { info with use = PowLoc.union info.use aset }
      | DEF -> { info with def = PowLoc.union info.def aset }
      | ALL -> { use = PowLoc.union info.use aset ;
                 def = PowLoc.union info.def aset }

    let from_set : kind -> PowLoc.t -> t
    = fun m aset -> add_set m aset empty
  
    let add_list : kind -> Loc.t list -> t -> t
    = fun m alist info -> list_fold (add m) alist info

    let accessof : t -> PowLoc.t
    = fun l -> PowLoc.union l.def l.use

    let useof : t -> PowLoc.t
    = fun l -> l.use

    let defof : t -> PowLoc.t
    = fun l -> l.def

    let union : t -> t -> t
    = fun l1 l2 -> 
      {
        use = PowLoc.union l1.use l2.use;
        def = PowLoc.union l1.def l2.def
      }

    let diff : t -> t -> PowLoc.t
    = fun l1 l2 -> 
      PowLoc.diff (accessof l1) (accessof l2)

    let restrict : PowLoc.t -> t -> t
    = fun addrs l -> 
      {
        use = PowLoc.inter l.use addrs;
        def = PowLoc.inter l.def addrs
      }

    let filter_out : PowLoc.t -> t -> t
    = fun addrs l -> 
      {
        use = PowLoc.diff l.use addrs;
        def = PowLoc.diff l.def addrs
      }

    let cardinal : t -> int
    = fun l -> PowLoc.cardinal (accessof l)

    let to_string_use : t -> string
    = fun l -> "Use = " ^ PowLoc.to_string l.use

    let to_string_def : t -> string
    = fun l -> "Def = " ^ PowLoc.to_string l.def

    let to_string : t -> string = fun l -> to_string_use l ^ "\n" ^ to_string_def l

    let print : t -> unit
    = fun l -> prerr_string (to_string l)

    let print_use : t -> unit
    = fun l -> prerr_endline (to_string_use l)

    let print_def : t -> unit
    = fun l -> prerr_endline (to_string_def l)
  end
  module ProcMap = BatMap.Make(Proc)
  module NodeMap = BatMap.Make(Node)
  module LocMap = BatMap.Make(Loc)
  type info = Info.t
  type t = {
    total_abslocs : PowLoc.t;
    access : info NodeMap.t;
    access_proc : info ProcMap.t;
    access_proc_reach : info ProcMap.t;
    access_proc_reach_wo_local : info ProcMap.t;
    access_proc_local : PowLoc.t ProcMap.t;
    access_program_local : PowLoc.t;
    def_nodes : PowNode.t LocMap.t;
    use_nodes : PowNode.t LocMap.t
  }

  let empty = {
    total_abslocs = PowLoc.empty;
    access = NodeMap.empty;
    access_proc = ProcMap.empty;
    access_proc_reach = ProcMap.empty;
    access_proc_reach_wo_local = ProcMap.empty;
    access_proc_local = ProcMap.empty;
    access_program_local = PowLoc.empty;
    def_nodes = LocMap.empty;
    use_nodes = LocMap.empty
  }

  let total_abslocs : t -> PowLoc.t
  =fun t -> t.total_abslocs

  let find_node : Node.t -> t -> info
  =fun n t -> try NodeMap.find n t.access with _ -> Info.empty

  let find_proc : Proc.t -> t -> info
  =fun pid t -> try ProcMap.find pid t.access_proc with _ -> Info.empty

  (* abstract locations exclusively accessed in the given function *)
  let find_proc_local : Proc.t -> t -> PowLoc.t
  =fun pid t -> try ProcMap.find pid t.access_proc_local with _ -> PowLoc.empty

  let find_program_local : t -> PowLoc.t
  =fun t -> t.access_program_local

  let find_proc_reach : Proc.t -> t -> info
  =fun pid t -> try ProcMap.find pid t.access_proc_reach with _ -> Info.empty

  let find_proc_reach_wo_local : Proc.t -> t -> info
  =fun pid t -> try ProcMap.find pid t.access_proc_reach_wo_local with _ -> Info.empty

  let find_def_nodes : Loc.t -> t -> PowNode.t
  =fun x t -> try LocMap.find x t.def_nodes with _ -> PowNode.empty

  let find_use_nodes : Loc.t -> t -> PowNode.t
  =fun x t -> try LocMap.find x t.use_nodes with _ -> PowNode.empty 

  let find_single_defs : t -> PowLoc.t
  =fun t -> 
    LocMap.fold (fun loc nodes -> 
      if PowNode.cardinal nodes = 1 then PowLoc.add loc else id
    ) t.def_nodes PowLoc.empty

  let restrict_access : t -> PowLoc.t -> t
  =fun t locs ->  
    { t with access = 
        NodeMap.fold (fun node access ->
          NodeMap.add node (Info.restrict locs access) 
        ) t.access NodeMap.empty }

  let add_node node info access = 
    { access with access = NodeMap.add node info access.access }

  let add_proc pid info access = 
    { access with access_proc = ProcMap.modify_def Info.empty pid
      (Info.union info) access.access_proc }

  let add_proc_reach pid info access = 
    { access with access_proc_reach = ProcMap.add pid info access.access_proc_reach }

  let add_proc_reach_wo_local pid info access = 
    { access with access_proc_reach_wo_local = ProcMap.add pid info access.access_proc_reach_wo_local }

  let add_proc_local pid loc access = 
    { access with access_proc_local = ProcMap.modify_def PowLoc.empty pid 
      (PowLoc.add loc) access.access_proc_local }

  let add_program_local access_program_local access = 
    { access with access_program_local }

  let add_def_nodes x nodes access = 
    { access with def_nodes = LocMap.add x nodes access.def_nodes }

  let add_use_nodes x nodes access = 
    { access with use_nodes = LocMap.add x nodes access.use_nodes }
   
  let add_total_abslocs total_abslocs access = { access with total_abslocs }
  let fold f access a = NodeMap.fold f access.access a
  let fold_proc f access a = ProcMap.fold f access.access_proc a
  let fold_proc_local f access a = ProcMap.fold f access.access_proc_local a
end
