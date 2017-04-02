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
(** Abstract semantics of octagon analysis *)
include AccessSem.S with type Dom.t = OctDom.Mem.t and type Dom.A.t = OctDom.Pack.t and type Dom.PowA.t = OctDom.PackConf.t

val check_bo: BasicDom.Proc.t -> OctDom.PackConf.t -> BasicDom.Allocsite.t 
  -> Itv.t -> Cil.exp -> ItvDom.Mem.t -> Dom.t -> Itv.t
