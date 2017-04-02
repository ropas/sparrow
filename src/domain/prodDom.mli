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
(** Product domain  *)
open AbsDom

module Make (A:LAT) (B:LAT) : sig
  include AbsDom.LAT
  val make  : A.t -> B.t -> t
  val fst   : t -> A.t
  val snd   : t -> B.t
end with type t = A.t * B.t

module Make5 (A:LAT) (B:LAT) (C:LAT) (D:LAT) (E:LAT) : sig
  include AbsDom.LAT
  val make  : (A.t * B.t * C.t * D.t * E.t) -> t
  val fst   : t -> A.t
  val snd   : t -> B.t
  val trd   : t -> C.t
  val frth  : t -> D.t
  val fifth : t -> E.t
end with type t = A.t * B.t * C.t * D.t * E.t
