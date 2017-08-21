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
(** Signature for abstract semantics *)

type update_mode =
  | Weak
  | Strong

module type S =
sig
  module Dom : InstrumentedMem.S
  module Spec : Spec.S with type Dom.t = Dom.t and type Dom.A.t = Dom.A.t
    and type Dom.PowA.t = Dom.PowA.t
  val run : update_mode -> Spec.t -> BasicDom.Node.t -> Dom.t * Global.t
    -> Dom.t * Global.t
  val initial : Dom.PowA.t -> Dom.t
end
