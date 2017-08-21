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
open Yojson.Safe
open Global
open IntraCfg
open InterCfg
open BasicDom

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

module Make (Dom : InstrumentedMem.S) =
struct
  type node = BasicDom.Node.t
  module PowLoc = Dom.PowA
  module Loc = Dom.A
  type loc = Loc.t
  type locset = PowLoc.t
  module G =
  struct
    module I = Graph.Imperative.Digraph.ConcreteBidirectional (BasicDom.Node)
    type t = { graph : I.t; label : ((node * node), locset) Hashtbl.t }
    let create ~size () = { graph = I.create ~size (); label = Hashtbl.create (2 * size) }

    let succ g n = I.succ g.graph n
    let pred g n = I.pred g.graph n
    let nb_vertex g = I.nb_vertex g.graph
    let pred_e g n = I.pred g.graph n |> List.map (fun p -> (p, Hashtbl.find g.label (p,n), n))
    let fold_vertex f g a = I.fold_vertex f g.graph a
    let fold_edges f g a = I.fold_edges f g.graph a
    let iter_edges f g = I.iter_edges f g.graph
    let fold_succ f g a = I.fold_succ f g.graph a

    let remove_vertex g n = I.remove_vertex g.graph n; g
    let add_edge g s d = I.add_edge g.graph s d; g
    let add_edge_e g (s,locs,d) =
      Hashtbl.replace g.label (s,d) locs;
      add_edge g s d
    let remove_edge g s d = I.remove_edge g.graph s d; Hashtbl.remove g.label (s,d); g
    let find_label g s d = Hashtbl.find g.label (s,d)
    let modify_edge_def def g s d f =
      try
        let old_label = find_label g s d in
        let new_label = f old_label in
        Hashtbl.replace g.label (s,d) new_label;
        g
      with _ ->
        add_edge_e g (s, def, d)
  end

  type t = G.t

  let create ?(size=0) () = G.create ~size ()
  let nodesof dug = G.fold_vertex BatSet.add dug BatSet.empty

  let succ n dug = try G.succ dug n with _ -> []
  let pred n dug = try G.pred dug n with _ -> []
  let nb_node dug = G.nb_vertex dug

  let remove_node : node -> t -> t
  =fun n dug -> G.remove_vertex dug n

  let add_edge : node -> node -> t -> t
  =fun src dst dug -> G.add_edge dug src dst

  let remove_edge : node -> node -> t -> t
  =fun src dst dug -> try G.remove_edge dug src dst with _ -> dug

  let get_abslocs : node -> node -> t -> locset
  =fun src dst dug -> try G.find_label dug src dst with _ -> PowLoc.empty

  let mem_duset : loc -> locset -> bool
  =fun x duset -> PowLoc.mem x duset

  let add_edge_e dug e = G.add_edge_e dug e

  let add_absloc : node -> Loc.t -> node -> t -> t
  =fun src x dst dug ->
    G.modify_edge_def (PowLoc.singleton x) dug src dst (PowLoc.add x)

  let add_abslocs : node -> locset -> node -> t -> t
  =fun src xs dst dug ->
    if PowLoc.is_empty xs then dug else
    G.modify_edge_def xs dug src dst (PowLoc.union xs)

  let fold_node = G.fold_vertex
  let fold_edges = G.fold_edges
  let iter_edges = G.iter_edges
  let fold_succ = G.fold_succ

  let nb_loc dug =
    fold_edges (fun src dst size ->
      PowLoc.cardinal (get_abslocs src dst dug) + size
    ) dug 0

  let succ_e : node -> t -> (node * locset) list
  =fun n g -> List.map (fun s -> (s, get_abslocs n s g)) (succ n g)

  let pred_e : node -> t -> (node * locset) list
  =fun n g -> List.map (fun p -> (p, get_abslocs p n g)) (pred n g)

  let to_dot : t -> string
  =fun dug ->
    "digraph dugraph {\n" ^
    fold_edges (fun src dst str ->
      let addrset = get_abslocs src dst dug in
        str ^ "\"" ^ (BasicDom.Node.to_string src) ^ "\"" ^ " -> " ^
              "\"" ^ (BasicDom.Node.to_string dst) ^ "\"" ^
              "[label=\"{" ^
                PowLoc.fold (fun addr s -> (Loc.to_string addr)^","^s) addrset "" ^
              "}\"]" ^ ";\n"
    ) dug ""
    ^ "}"

  let to_json : t -> json
  = fun g ->
    let nodes = `List (fold_node (fun v nodes ->
                  (`String (BasicDom.Node.to_string v))::nodes) g [])
    in
    let edges = `List (fold_edges (fun src dst edges ->
                  let addrset = get_abslocs src dst g in
                  (`List [`String (BasicDom.Node.to_string src); `String (BasicDom.Node.to_string dst);
                          `String (PowLoc.fold (fun addr s -> (Loc.to_string addr)^","^s) addrset "")])
                  ::edges) g [])
    in
    `Assoc [("nodes", nodes); ("edges", edges)]
end
