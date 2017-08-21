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
(** Sparse analysis framework *)
module type S =
sig
  module Dom : InstrumentedMem.S
  module Table : MapDom.CPO with type t = MapDom.MakeCPO(BasicDom.Node)(Dom).t and type A.t = BasicDom.Node.t and type B.t = Dom.t
  module Spec : Spec.S with type Dom.t = Dom.t and type Dom.A.t = Dom.A.t and type Dom.PowA.t = Dom.PowA.t
  val perform : Spec.t -> Global.t -> Global.t * Table.t * Table.t
end

module Make (Sem:AbsSem.S) : S
  with type Dom.t = Sem.Dom.t
  and type Dom.A.t = Sem.Dom.A.t
  and type Dom.PowA.t = Sem.Dom.PowA.t

module MakeWithAccess (Sem:AccessSem.S) : S
  with type Dom.t = Sem.Dom.t
  and type Dom.A.t = Sem.Dom.A.t
  and type Dom.PowA.t = Sem.Dom.PowA.t
