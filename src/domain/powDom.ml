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
open AbsDom
open Vocab

module type S = 
sig 
  include LAT
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

module PolySet = BatSet 

module Make (A:SET) =
struct
  module BatSet = BatSet.Make(A)
  type elt = A.t
  type t = BatSet.t
  
  exception Error
  let compare = BatSet.compare

  let to_string : t -> string = fun x ->
    if BatSet.is_empty x then "bot" else
      let add_string_of_v v acc = link_by_sep "," (A.to_string v) acc in
      "{" ^ BatSet.fold add_string_of_v x "" ^ "}"

  let le : t -> t -> bool = fun x y ->
    if x == y then true else BatSet.subset x y

  let eq : t -> t -> bool = fun x y ->
    if x == y then true else BatSet.equal x y

  let bot : t = BatSet.empty
  let empty = bot

  let join : t -> t -> t = fun x y ->
    if le x y then y else
    if le y x then x else
      BatSet.union x y
  let union = join 
  let union_small_big small big = BatSet.fold BatSet.add small big

  let meet : t -> t -> t = fun x y ->
    if le x y then x else
    if le y x then y else
      BatSet.inter x y
  let inter = meet

  (* Since module A is finite,  widening is defined as union which is
     sufficient to guarantee analysis termination.  *)
  let widen : t -> t -> t = fun x y ->
    if x == y then x else
      BatSet.union x y

  let narrow : t -> t -> t = fun x y ->
    if x == y then x else
      BatSet.inter x y


  let filter : (elt -> bool) -> t -> t = fun f s ->
    BatSet.filter f s

  let fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a = BatSet.fold
  let fold2 : (elt -> elt -> 'a -> 'a) -> t -> t -> 'a -> 'a 
  = fun f s1 s2 -> BatSet.fold (fun x -> BatSet.fold (f x) s2) s1

  let map = BatSet.map

  let iter : (elt -> unit) -> t -> unit = BatSet.iter

  let singleton : elt -> t = fun e ->
    BatSet.singleton e

  let subset : t -> t -> bool = BatSet.subset

  let cardinal : t -> int = BatSet.cardinal

  let mem : elt -> t -> bool = BatSet.mem

  let add e s = BatSet.add e s
  let diff = BatSet.diff

  let choose = BatSet.choose
  
  let remove = BatSet.remove

  let elements = BatSet.elements
  let is_empty = BatSet.is_empty

  let for_all = BatSet.for_all
  let exists = BatSet.exists
  let of_list = BatSet.of_list
end
