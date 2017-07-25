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
(** Def-use graph *)
module type S = 
sig
  type t
  type node = BasicDom.Node.t
  module Loc : AbsDom.SET
  module PowLoc : PowDom.CPO with type elt = Loc.t

  val create            : ?size : int -> unit -> t 
  val nb_node           : t -> int
  val nb_loc            : t -> int
  val nodesof           : t -> node BatSet.t

  val succ              : node -> t -> node list
  val pred              : node -> t -> node list

  val add_edge          : node -> node -> t -> t
  val remove_node       : node -> t -> t
  val get_abslocs       : node -> node -> t -> PowLoc.t 
  val mem_duset         : Loc.t -> PowLoc.t -> bool
  val add_absloc        : node -> Loc.t -> node -> t -> t
  val add_abslocs       : node -> PowLoc.t -> node -> t -> t

(** {2 Iterator } *)

  val fold_node         : (node -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_edges        : (node -> node -> 'a -> 'a) -> t -> 'a -> 'a 
  val iter_edges        : (node -> node -> unit) -> t -> unit
  val fold_succ         : (node -> 'a ->'a) -> t -> node -> 'a -> 'a

(** {2 Print } *)

  val to_dot            : t -> string
  val to_json           : t -> Yojson.Safe.json
end

module Make (Dom : InstrumentedMem.S) : S 
  with type Loc.t = Dom.A.t and type PowLoc.t = Dom.PowA.t
