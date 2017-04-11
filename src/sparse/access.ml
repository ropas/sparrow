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
open Vocab
open AbsDom
open BasicDom

module type S = 
sig
  type t
  module Loc : SET
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

module Make(Dom: MapDom.CPO) = 
struct
  module PowLoc = Dom.PowA
  module Loc = Dom.A

  type mode = DEF | USE | ALL

  type t = {
    def    : PowLoc.t;
    use    : PowLoc.t
  }
  let def = DEF
  let use = USE
  let all = ALL
  let empty = {
    def    = PowLoc.empty;
    use    = PowLoc.empty
  }

  let add : mode -> Loc.t -> t -> t
  = fun m a t ->
    match m with
    | USE -> { t with use = PowLoc.add a t.use }
    | DEF -> { t with def = PowLoc.add a t.def }
    | ALL -> { use = PowLoc.add a t.use;
               def = PowLoc.add a t.def }

  let singleton : mode -> Loc.t -> t
  = fun m a -> add m a empty
   
  let mem : Loc.t -> t -> bool
  = fun l a ->
    (PowLoc.mem l a.def) || (PowLoc.mem l a.use)

  let remove : Loc.t -> t -> t
  = fun a t ->
    {
      use = PowLoc.remove a t.use;
      def = PowLoc.remove a t.def;
    } 

  let remove_set : PowLoc.t -> t -> t
  = fun addrs t ->
    { 
      use = PowLoc.diff t.use addrs;
      def = PowLoc.diff t.def addrs;
    }

  let add_set : mode -> PowLoc.t -> t -> t
  = fun m aset t ->
    match m with
    | USE -> { t with use = PowLoc.union t.use aset }
    | DEF -> { t with def = PowLoc.union t.def aset }
    | ALL -> { use = PowLoc.union t.use aset ;
               def = PowLoc.union t.def aset }

  let from_set : mode -> PowLoc.t -> t
  = fun m aset -> add_set m aset empty
  
  let add_list : mode -> Loc.t list -> t -> t
  = fun m alist t -> list_fold (add m) alist t

  let accessof : t -> PowLoc.t
  = fun l -> PowLoc.union l.def l.use

  let useof : t -> PowLoc.t
  = fun l -> l.use

  let defof : t -> PowLoc.t
  = fun l -> l.def

  let union : t -> t -> t
  = fun l1 l2 -> 
    {
      use = PowLoc.union l1.use l2.use;
      def = PowLoc.union l1.def l2.def
    }

  let diff : t -> t -> PowLoc.t
  = fun l1 l2 -> 
    PowLoc.diff (accessof l1) (accessof l2)

  let restrict : PowLoc.t -> t -> t
  = fun addrs l -> 
    {
      use = PowLoc.inter l.use addrs;
      def = PowLoc.inter l.def addrs
    }

  let filter_out : PowLoc.t -> t -> t
  = fun addrs l -> 
    {
      use = PowLoc.diff l.use addrs;
      def = PowLoc.diff l.def addrs
    }

  let cardinal : t -> int
  = fun l -> PowLoc.cardinal (accessof l)

  let to_string_use : t -> string
  = fun l -> "Use = " ^ PowLoc.to_string l.use

  let to_string_def : t -> string
  = fun l -> "Def = " ^ PowLoc.to_string l.def

  let to_string : t -> string = fun l -> to_string_use l ^ "\n" ^ to_string_def l

  let print : t -> unit
  = fun l -> prerr_string (to_string l)

  let print_use : t -> unit
  = fun l -> prerr_endline (to_string_use l)

  let print_def : t -> unit
  = fun l -> prerr_endline (to_string_def l)
end
