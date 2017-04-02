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
(** Abstract Domain *)

open Vocab
open InterCfg

module Node = InterCfg.Node
module PowNode = PowDom.Make(Node)
module Proc = InterCfg.Proc
module PowProc = PowDom.Make (Proc)

module ExtAllocsite =
struct
  type t = Input | Unknown of string [@@deriving compare]

  let input = Input
  let unknown s = Unknown s
  let is_cmd_arg x = 
    match x with 
      Unknown s -> s = "arg"
    | _ -> false
  let to_string = function
    | Input -> "__extern__"
    | Unknown s -> "__extern__" ^ s
end

module IntAllocsite = 
struct 
  type t = Node.t * is_string [@@deriving compare]
  and is_string = bool   
  
  let to_string (node,_) = Node.to_string node
end

module Allocsite = 
struct 
  type t = Internal of IntAllocsite.t | External of ExtAllocsite.t [@@deriving compare]
  let allocsite_of_node : Node.t -> t 
  = fun n -> Internal (n,false)
  let allocsite_of_string : Node.t -> t 
  = fun n -> Internal (n,true)

  let is_node_allocsite = function Internal (_,false) -> true | _ -> false
  let is_string_allocsite = function Internal (_,true) -> true | _ -> false
  let is_ext_allocsite = function External _ -> true | _ -> false
  let is_cmd_arg = function External e -> ExtAllocsite.is_cmd_arg e | _ -> false

  let allocsite_of_ext : string option -> t 
  = function None -> External (ExtAllocsite.input)
  | Some fid -> External (ExtAllocsite.unknown fid)

  let to_string 
  = function Internal i -> IntAllocsite.to_string i
  | External e -> ExtAllocsite.to_string e
end

module Loc = 
struct 
  type t = GVar of string * Cil.typ | LVar of Proc.t * string * Cil.typ | Allocsite of Allocsite.t 
  | Field of t * field * Cil.typ
  and field = string 

  let rec compare x y = 
    match x, y with
    | GVar (g1, _), GVar (g2, _) -> String.compare g1 g2
    | LVar (p1, l1, _), LVar (p2, l2, _) -> 
      let c = Proc.compare p1 p2 in
      if c = 0 then String.compare l1 l2 else c
    | Allocsite a1, Allocsite a2 -> Allocsite.compare a1 a2
    | Field (l1, f1, _), Field (l2, f2, _) -> 
      let c = compare l1 l2 in
      if c = 0 then String.compare f1 f2 else c
    | _, _ -> Pervasives.compare (tag_of_t x) (tag_of_t y)
  and tag_of_t = function GVar _ -> 0 | LVar _ -> 1 | Allocsite _ -> 2 | Field _ -> 3

  let typ = function GVar (_, t) | LVar (_, _, t) | Field (_, _, t) -> Some t | _ -> None

  let rec to_string = function 
    | GVar (g, _) -> g
    | LVar (p, x, _) -> "(" ^ Proc.to_string p ^ "," ^ x ^ ")"
    | Allocsite a -> Allocsite.to_string a
    | Field (a, f, _) -> to_string a ^ "." ^ f

  let dummy = GVar ("__dummy__", Cil.voidType)
  let null = GVar ("NULL", Cil.voidPtrType)

  let is_null x = (x = null)
  let is_var : t -> bool = function 
    | GVar _ | LVar _ -> true
    | _ -> false
   
  let is_gvar : t -> bool = function
    | GVar _ -> true
    | _ -> false

  let is_lvar : t -> bool = function
    | LVar _ -> true
    | _ -> false 

  let is_allocsite : t -> bool = function
    | Allocsite _ -> true
    | _ -> false

  let is_string_allocsite : t -> bool = function
    | Allocsite a -> Allocsite.is_string_allocsite a
    | _ -> false
  
  let is_ext_allocsite : t -> bool = function 
    | Allocsite a -> Allocsite.is_ext_allocsite a
    | _ -> false  

  let is_field : t -> bool = function
    | Field _ -> true
    | _ -> false

  let is_local_of : Proc.t -> t -> bool = fun p x ->
    match x with 
    | LVar (p',_,_) -> p = p'
    | _ -> false

  let get_proc : t -> Proc.t 
  = function LVar (p, _, _) -> p | _ -> raise Not_found

  let of_gvar x typ = GVar (x,typ)
  let of_lvar p x typ = LVar (p,x,typ) 
  let of_allocsite : Allocsite.t -> t = fun x -> Allocsite x 

  let append_field x f typ = Field (x,f,typ)
end

module PowLoc = 
struct 
  include PowDom.Make (Loc)

  let prune op x e = 
    match op with 
      Cil.Eq when Cil.isZero e -> singleton Loc.null
    | Cil.Ne when Cil.isZero e -> remove Loc.null x
    | _ -> x
  let null = singleton Loc.null
  let append_field : t -> Cil.fieldinfo -> t = fun ls f ->
    let add_appended l acc = 
      if Loc.is_ext_allocsite l then add l acc
      else if Loc.is_null l || Loc.is_string_allocsite l then acc 
      else add (Loc.append_field l f.Cil.fname f.Cil.ftype) acc
    in
    fold add_appended ls bot
end

module Dump = MapDom.Make (Proc) (PowLoc)
