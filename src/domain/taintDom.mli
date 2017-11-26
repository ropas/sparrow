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
(** Abstract domains of taint analysis *)
module IntOverflow : sig
  type t = Bot | Top
  include AbsDom.LAT with type t := t
  val is_bot : t -> bool
end

module UserInput : sig
  module Source : sig type t = BasicDom.Node.t * Cil.location end
  include PowDom.LAT with type elt = Source.t
  val is_taint : t -> bool
end

module Val : sig
  type t = {
    int_overflow : IntOverflow.t;
    user_input : UserInput.t;
  }
  include AbsDom.LAT with type t := t
  val int_overflow : t -> IntOverflow.t
  val user_input : t -> UserInput.t
  val input_value : BasicDom.Node.t -> Cil.location -> t
end

module Mem : sig
  include InstrumentedMem.S
  val lookup : BasicDom.PowLoc.t -> t -> Val.t
  val strong_update : BasicDom.PowLoc.t -> Val.t -> t -> t
  val weak_update : BasicDom.PowLoc.t -> Val.t -> t -> t
end with type A.t = BasicDom.Loc.t and type B.t = Val.t and type PowA.t = BasicDom.PowLoc.t
