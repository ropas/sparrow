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
(** Abstract semantics of taint analysis *)
include AbsSem.S with type Dom.t = TaintDom.Mem.t and type Dom.A.t = BasicDom.Loc.t and type Dom.PowA.t = BasicDom.PowLoc.t

val eval : BasicDom.Proc.t -> Cil.exp -> ItvDom.Mem.t -> TaintDom.Mem.t -> TaintDom.Val.t
