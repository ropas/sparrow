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
(** Powset domain *)

module type S = 
sig 
  include AbsDom.LAT
  type elt
  exception Error

  val empty : t
  val filter : (elt -> bool) -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val fold2 : (elt -> elt -> 'a -> 'a) -> t -> t -> 'a -> 'a
  val map : (elt -> elt) -> t -> t
  val iter : (elt -> unit) -> t -> unit

  val singleton : elt -> t 
  val subset : t -> t -> bool
  val cardinal : t -> int

  val mem : elt -> t -> bool

  val add : elt -> t -> t
  val diff : t -> t -> t

  val choose : t -> elt
  
  val remove : elt -> t -> t

  val is_empty : t -> bool

  val union : t -> t -> t
  val union_small_big : t -> t -> t
  val inter : t -> t -> t
  val for_all : (elt -> bool) -> t -> bool 
  val exists : (elt -> bool) -> t -> bool
  val of_list : elt list -> t
  val elements : t -> elt list
end

module Make (A:AbsDom.SET) : S with type elt = A.t and type t = BatSet.Make(A).t
