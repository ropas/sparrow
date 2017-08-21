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
open AbsDom
open ProdDom
open BasicDom
open Vocab

module Struct =
struct
  include String
  let to_string = id
  let pp fmt x = Format.fprintf fmt "%s" x
end

module PowStruct = PowDom.MakeCPO (Struct)

include MapDom.MakeLAT (Loc) (PowStruct)

let make : PowLoc.t -> Cil.compinfo -> t
= fun ploc s ->
  PowLoc.fold (fun l ->
    add l (PowStruct.singleton s.Cil.cname)) ploc bot

let append_field : t -> Cil.fieldinfo -> PowLoc.t
= fun s f ->
  foldi (fun loc info ->
      if PowStruct.mem f.Cil.fcomp.Cil.cname info then
        PowLoc.add (Loc.append_field loc f.Cil.fname f.Cil.ftype)
      else id) s PowLoc.bot

let pow_loc_of_struct : t -> PowLoc.t = fun str ->
  foldi (fun k _ -> PowLoc.add k) str PowLoc.bot

let extern () =
  if !Options.top_location then top
  else bot

let to_string : t -> string = fun x ->
  if is_empty x then "bot" else
  foldi (fun a b s ->
      let str = A.to_string a ^ " -> " ^ B.to_string b in
      link_by_sep "\n\t" str s) x ""
