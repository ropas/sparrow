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
  module PowLoc : PowDom.CPO
  module Access : Access.S with type Loc.t = Loc.t and type PowLoc.t = PowLoc.t
  val perform : Global.t -> PowLoc.t -> (BasicDom.Node.t -> Dom.t * Global.t -> Dom.t * Global.t) -> Dom.t -> Access.t
end

module Make(AccessSem : AccessSem.S) : S 
  with type Dom.t = AccessSem.Dom.t 
  and type Dom.A.t = AccessSem.Dom.A.t 
  and type Dom.PowA.t = AccessSem.Dom.PowA.t 
  and type Loc.t = AccessSem.Dom.A.t
  and type PowLoc.t = AccessSem.Dom.PowA.t
  and type Access.t = AccessSem.Dom.Access.t
