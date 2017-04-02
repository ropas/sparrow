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
(** Abstract domains of octagon analysis *)

module OctLoc : sig
  type t = Loc of BasicDom.Loc.t | Size of BasicDom.Allocsite.t
  include AbsDom.SET with type t := t
  val dummy : t 
  val of_loc : BasicDom.Loc.t -> t
  val of_size : BasicDom.Allocsite.t -> t
  val to_var : t -> Apron.Var.t
end

module PowOctLoc : sig
  include PowDom.S
  val empty: t
  val of_locs : BasicDom.PowLoc.t -> t
  val of_sizes : BasicDom.Allocsite.t BatSet.t -> t
end with type elt = OctLoc.t

module Pack = PowOctLoc

module PackConf : sig
  include PowDom.S
  val empty : t
  val make : ItvAnalysis.Table.t -> t -> t
  val get_pack : t -> OctLoc.t -> Pack.t
  val singleton : Pack.t -> t
  val add : Pack.t -> t -> t
  val fold : (Pack.t -> 'a -> 'a) -> t -> 'a -> 'a
  val print_info : t -> unit
end with type t = PowDom.Make(Pack).t and type elt = Pack.t

module Octagon : sig
 include AbsDom.LAT
 val is_bot : t -> bool
 val itv_of_var : OctLoc.t -> t -> Itv.t
 val itv_of_expr : Apron.Texpr1.expr -> t -> Itv.t

 val set : OctLoc.t -> Apron.Texpr1.expr -> t -> t
 val forget : OctLoc.t -> t -> t
 val prune : OctLoc.t -> Apron.Texpr1.expr -> Apron.Tcons1.typ -> t -> t
end

module Mem : sig
  include InstrumentedMem.S
  val top: PackConf.t -> t
end with type A.t = Pack.t and type B.t = Octagon.t and type PowA.t = PackConf.t
