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
(** Accessibility information *)
module type S = 
sig
  module Loc : AbsDom.SET
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
  val add_node : BasicDom.Node.t -> info -> t -> t
  val add_proc : BasicDom.Proc.t -> info -> t -> t
  val add_proc_reach : BasicDom.Proc.t -> info -> t -> t
  val add_proc_local : BasicDom.Proc.t -> Loc.t -> t -> t
  val add_proc_reach_wo_local : BasicDom.Proc.t -> info -> t -> t
  val add_program_local : PowLoc.t -> t -> t
  val add_def_nodes : Loc.t -> BasicDom.PowNode.t -> t -> t
  val add_use_nodes : Loc.t -> BasicDom.PowNode.t -> t -> t
  val add_total_abslocs : PowLoc.t -> t -> t
  val fold : (BasicDom.Node.t -> info -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_proc : (BasicDom.Proc.t -> info -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_proc_local : (BasicDom.Proc.t -> PowLoc.t -> 'a -> 'a) -> t -> 'a -> 'a
  val total_abslocs : t -> PowLoc.t
  val find_node : BasicDom.Node.t -> t -> info
  val find_proc : BasicDom.Proc.t -> t -> info
  val find_proc_reach : BasicDom.Proc.t -> t -> info
  val find_proc_reach_wo_local : BasicDom.Proc.t -> t -> info
  val find_proc_local : BasicDom.Proc.t -> t -> PowLoc.t
  val find_program_local : t -> PowLoc.t
  val find_def_nodes : Loc.t -> t -> BasicDom.PowNode.t
  val find_use_nodes : Loc.t -> t -> BasicDom.PowNode.t
  val find_single_defs : t -> PowLoc.t
  val restrict_access : t -> PowLoc.t -> t
end

module Make(Dom: MapDom.CPO) : S with type PowLoc.t = Dom.PowA.t and type Loc.t = Dom.A.t
