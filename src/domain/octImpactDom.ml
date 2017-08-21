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
(* Octagon Impact Domain *)
open Cil
open BasicDom
open AbsDom
open OctDom
open AbsSem
open Vocab

module AbsOct =
struct
  module Node = struct include OctLoc let equal = ( = ) let hash = Hashtbl.hash end
  module G = Graph.Persistent.Digraph.ConcreteBidirectional (Node)
  module W =
  struct
    type edge = G.edge
    type t = int [@@deriving compare]
    let weight _ = 1
    let add = ( + )
    let zero = 0
  end
  module D = Graph.Path.Dijkstra(G)(W)
  module Check = Graph.Path.Check(G)

  type t = Bot | V of G.t * PowOctLoc.t * PowOctLoc.t  (* relation * const * defined *)
  let bot = Bot
  let top = V (G.empty, PowOctLoc.bot, PowOctLoc.bot)
  let fold f g a = match g with Bot -> a | V (g, _, _) -> G.fold_edges f g a

  let is_defined x = function Bot -> false | V (_, _, d) -> PowOctLoc.mem x d

  let closure = function Bot -> bot | V (g, c, d) ->
    let path_checker = Check.create g in
    V (G.fold_vertex (fun s g ->
       G.fold_vertex (fun d g ->
         if s = d then g
         else if Check.check_path path_checker s d then
           G.add_edge g s d
         else g) g g) g g, c, d)
  let cardinal = function Bot -> -1 | V (g,_,_) -> G.nb_edges g
  let remove x y = function Bot -> bot | V (g,c,d) -> V (G.remove_edge g x y, c,d)

  (* idx - size <= * *)
  let check : OctLoc.t -> OctLoc.t -> t -> bool
  = fun idx size o ->
    match o with
    | Bot -> false
    | V (g, _, _) ->
      let path_checker = Check.create g in
      Check.check_path path_checker size idx

  let get_pack idx size = function
      Bot -> None
    | V (o, c, _) ->
      try
        let (e,_) = D.shortest_path o size idx in
        Some (List.fold_left (fun s e ->
            s |> Pack.add (G.E.src e) |> Pack.add (G.E.dst e))
          Pack.bot e)
      with _ -> None

  (* x := c *)
  let set_const x = function Bot -> bot | V (g,c,d) -> V (g, PowOctLoc.add x c, d |> PowOctLoc.add x)
  let weak_set_const x = function Bot -> bot | V (g,c,d) -> V (g, c, d |> PowOctLoc.add x)

  (* x := y + c *)
  let set_variable x y = function Bot -> bot | V (g,c,d) ->
    if OctLoc.compare x y = 0 then V (g,c,d)
    else if PowOctLoc.mem y d then
      let g = G.add_edge g x y in
      let g = G.add_edge g y x in
      V (g, c, PowOctLoc.add x d) (* |> closure *)
    else V (g,c,d)

  let weak_set_variable x y = function Bot -> bot | V (g,c,d) ->
    if OctLoc.compare x y = 0 then V (g,c,d)
    else if PowOctLoc.mem y d then
      V (g, c, PowOctLoc.add x d) (* |> closure *)
    else V (g,c,d)

  (* x := ? *)
  let forget x = function Bot -> bot | V (g,c,d) -> V  (G.remove_vertex g x, PowOctLoc.remove x c,d |> PowOctLoc.add x)

  (* x - y <= c *)
  let assume x y = function Bot -> bot | V (g,c,d) ->
    if OctLoc.compare x y = 0 then V (g,c,d)
    else if PowOctLoc.mem x d && PowOctLoc.mem y d then
      let g = G.add_edge g y x in
      V (g, c, d) (* |> closure *)
    else V (g,c,d)

  let weak_assume x y = id

  let equal = ( = )

  let le x y =
    match x, y with
      Bot, _ -> true
    | _, Bot -> false
    | V (x, c1, d1), V (y, c2, d2) ->
      (PowOctLoc.le d1 d2) &&
      (try
        G.fold_edges (fun s d b ->
            if G.mem_edge x s d then true
            else raise Not_found) y true
       with Not_found -> false) && (PowOctLoc.le c2 c1)

  let extend (o1,d1) (o2,d2) =
    G.fold_edges (fun s d o1 ->
        if (PowOctLoc.mem s d1) && (PowOctLoc.mem d d1) then o1
        else G.add_edge o1 s d) o2 o1

  let join x y =
    match x, y with
      Bot, _ -> y
    | _, Bot -> x
    | V (x, c1, d1), V (y, c2, d2) ->
      let (x,y) =
        if PowOctLoc.eq d1 d2 then (x,y)
        else (extend (x,d1) (y,d2), extend (y,d2) (x,d1))
      in
      let newg = G.fold_edges (fun s d y ->
          if G.mem_edge x s d then y
          else G.remove_edge y s d) y y
      in
      V (newg,
         PowOctLoc.meet c1 c2,
         PowOctLoc.join d1 d2)

  let meet x y =
     match x, y with
      Bot, _ | _, Bot -> Bot
    | V (x,c1,d1), V (y,c2,d2) ->
      let (x,y) =
        if PowOctLoc.eq d1 d2 then (x,y)
        else (extend (x,d1) (y,d2), extend (y,d2) (x,d1))
      in
      let newg = G.fold_edges (fun s d y ->
          G.add_edge y s d) x y
      in
      V (newg,
         PowOctLoc.join c1 c2,
         PowOctLoc.join d1 d2)
  let eq = ( = )
  let compare = compare

  let widen = join
  let narrow = meet

  let is_empty x = x = bot
  let to_string = function
    | Bot -> "bot"
    | V (x, _, _) ->
      (G.fold_edges (fun s d str ->
           str^Node.to_string d ^ " - " ^ Node.to_string s ^"\n") x "")
      (*  ^"{"^(PowOctLoc.fold (fun x s -> s ^ OctLoc.to_string x ^ ",") d "")^"}\n"*)

  let pp fmt = function
    | Bot -> Format.fprintf fmt "bot"
    | V (x, _, _) ->
      G.iter_edges (fun s d ->
          Format.fprintf fmt "@[<hov 2>%a - %a,@]" Node.pp s Node.pp d) x
end

module Mem =
struct
  include InstrumentedMem.Make (MapDom.MakeCPO (Pack) (AbsOct))
  let init packconf =
    PackConf.fold (fun pack -> add pack AbsOct.top) packconf bot

  let top = init

  let cardinal mem =
    try
      choose mem |> snd |> AbsOct.cardinal
    with _ -> 0

  let lookup mem =
    try
      choose mem |> snd
    with _ -> AbsOct.bot

  let to_string m =
    try
      let (_, v) = choose m in
      AbsOct.to_string v
    with _ -> "Bot"
end

module Relation =
struct
  module Node = struct include OctLoc let equal = ( = ) let hash = Hashtbl.hash end
  module G = Graph.Persistent.Graph.Concrete (Node)
  module C = Graph.Components.Undirected (G)
  type t = G.t

  let empty = G.empty
  let add_edge x y g = G.add_edge g x y
  let add_absoct : AbsOct.t -> t -> t
  = fun aoct g ->
    AbsOct.fold (fun src dst g ->
        G.add_edge g src dst) aoct g

  let get_packconf : t -> PackConf.t
  = fun g ->
    let clist = C.components_list g in
    List.fold_left (fun packconf l ->
      let pack = list_fold Pack.add l Pack.empty in
      PackConf.add pack packconf) PackConf.empty clist
end

let pack = Pack.singleton OctLoc.dummy
let packconf = PackConf.singleton pack
