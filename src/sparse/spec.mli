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
module type S =
sig
  module Dom : InstrumentedMem.S

  type t = {
    locset : Dom.PowA.t;
    locset_fs : Dom.PowA.t;
    ptrinfo : ItvDom.Table.t;
    premem : Dom.t;
    (* unsoundness *)
    unsound_lib : string BatSet.t;
    unsound_update : bool;
    unsound_bitwise : bool;
  }
  val empty : t
end

module Make(Dom: InstrumentedMem.S) : S
  with type Dom.t = Dom.t and type Dom.A.t = Dom.A.t
  and type Dom.PowA.t = Dom.PowA.t
