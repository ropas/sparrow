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
(** Pre-analysis to compute accessibility *)

module type S = 
sig
  module Dom : InstrumentedMem.S
  module Loc : AbsDom.SET
  module PowLoc : PowDom.S
  module LocMap : BatMap.S with type key = Loc.t
  module Access : Access.S with type Loc.t = Loc.t and type PowLoc.t = PowLoc.t
  type t
  val empty : t
  val get_total_abslocs : t -> PowLoc.t
  val get_access : t -> BasicDom.Node.t -> Access.t
  val get_access_proc : t -> BasicDom.Proc.t -> Access.t
  val get_access_reach : t -> BasicDom.Proc.t -> Access.t
  val get_access_reach_wo_local : t -> BasicDom.Proc.t -> Access.t
  val get_access_local : t -> BasicDom.Proc.t -> PowLoc.t
  val get_access_local_program : t -> PowLoc.t
  val get_defs_of : t -> BasicDom.PowNode.t LocMap.t
  val get_uses_of : t -> BasicDom.PowNode.t LocMap.t
  val get_single_defs : BasicDom.PowNode.t LocMap.t -> PowLoc.t
  val restrict_access   : t -> PowLoc.t -> t
  val perform : Global.t -> PowLoc.t -> (BasicDom.Node.t -> Dom.t * Global.t -> Dom.t * Global.t) -> Dom.t -> t
end

module Make(AccessSem : AccessSem.S) : S 
  with type Dom.t = AccessSem.Dom.t 
  and type Dom.A.t = AccessSem.Dom.A.t 
  and type Dom.PowA.t = AccessSem.Dom.PowA.t 
  and type Loc.t = AccessSem.Dom.A.t
  and type PowLoc.t = AccessSem.Dom.PowA.t
