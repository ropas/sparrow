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
(** Accessibility information *)
module type S = 
sig
  type t
  module Loc : AbsDom.SET
  module PowLoc : PowDom.CPO with type elt = Loc.t
  type mode
  val empty : t
  val def : mode
  val use : mode
  val all : mode
  val add : mode -> Loc.t -> t -> t
  val singleton : mode -> Loc.t -> t
  val mem : Loc.t -> t -> bool
  val remove : Loc.t -> t -> t
  val remove_set : PowLoc.t -> t -> t
  val add_set : mode -> PowLoc.t -> t -> t
  val from_set : mode -> PowLoc.t -> t
  val add_list : mode -> Loc.t list -> t -> t
  val union : t -> t -> t
  val diff : t -> t -> PowLoc.t
  val restrict : PowLoc.t -> t -> t
  val filter_out : PowLoc.t -> t -> t
  val accessof : t -> PowLoc.t
  val useof : t -> PowLoc.t
  val defof : t -> PowLoc.t
  val cardinal : t -> int
  val to_string_use : t -> string
  val to_string_def : t -> string
  val to_string : t -> string
  val print : t -> unit
  val print_use : t -> unit
  val print_def : t -> unit
end

module Make(Dom: MapDom.CPO) : S with type PowLoc.t = Dom.PowA.t and type Loc.t = Dom.A.t
