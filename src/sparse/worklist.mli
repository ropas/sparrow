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
(** Worklist *)

module type S =
sig
  module DUGraph : Dug.S
  type t
  val init : DUGraph.t -> t
  val pick : t -> (BasicDom.Node.t * t) option
  val push : BasicDom.Node.t -> BasicDom.Node.t -> t -> t
  val push_set : BasicDom.Node.t -> BasicDom.Node.t BatSet.t -> t -> t
  val is_loopheader : BasicDom.Node.t -> t -> bool
end

module Make(DUGraph : Dug.S) : S with type DUGraph.t = DUGraph.t
