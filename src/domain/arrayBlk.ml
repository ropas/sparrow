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
(* Abstract Array Block *)

open AbsDom
open ProdDom
open BasicDom
open Vocab
open StructBlk

module ArrInfo =
struct
  type t = {
    offset    : Itv.t;
    size      : Itv.t;
    stride    : Itv.t;
    null_pos  : Itv.t;
    structure : PowStruct.t;
  } [@@deriving compare]

  let bot = { offset = Itv.bot; size = Itv.bot; stride = Itv.bot; null_pos = Itv.bot; structure = PowStruct.bot }
  let top = { offset = Itv.top; size = Itv.top; stride = Itv.top; null_pos = Itv.top; structure = PowStruct.bot }
  let input = { offset = Itv.zero; size = Itv.pos; stride = Itv.one; null_pos = Itv.nat; structure = PowStruct.bot }

  let make (o,s,stride,null,structure) =
    { offset = o; size = s; stride = stride; null_pos = null; structure = structure }

  let join a1 a2 =
    if a1 == a2 then a2 else
    { offset    = Itv.join a1.offset a2.offset;
      size      = Itv.join a1.size a2.size;
      stride    = Itv.join a1.stride a2.stride;
      null_pos  = Itv.join a1.null_pos a2.null_pos;
      structure = PowStruct.join a1.structure a2.structure; }

  let meet a1 a2 =
    if a1 == a2 then a2 else
    { offset    = Itv.meet a1.offset a2.offset;
      size      = Itv.meet a1.size a2.size;
      stride    = Itv.meet a1.stride a2.stride;
      null_pos  = Itv.meet a1.null_pos a2.null_pos;
      structure = PowStruct.meet a1.structure a2.structure; }

  let widen a1 a2 =
    if a1 == a2 then a2 else
    { offset    = Itv.widen a1.offset a2.offset;
      size      = Itv.widen a1.size a2.size;
      stride    = Itv.widen a1.stride a2.stride;
      null_pos  = Itv.widen a1.null_pos a2.null_pos;
      structure = PowStruct.widen a1.structure a2.structure; }

  let narrow a1 a2 =
    if a1 == a2 then a2 else
    { offset    = Itv.narrow a1.offset a2.offset;
      size      = Itv.narrow a1.size a2.size;
      stride    = Itv.narrow a1.stride a2.stride;
      null_pos  = Itv.narrow a1.null_pos a2.null_pos;
      structure = PowStruct.narrow a1.structure a2.structure; }

  let eq a1 a2 =
    if a1 == a2 then true
    else
      Itv.eq a1.offset a2.offset
      && Itv.eq a1.size a2.size
      && Itv.eq a1.stride a2.stride
      && Itv.eq a1.null_pos a2.null_pos
      && PowStruct.eq a1.structure a2.structure

  let le a1 a2 =
    if a1 == a2 then true
    else
      Itv.le a1.offset a2.offset
      && Itv.le a1.size a2.size
      && Itv.le a1.stride a2.stride
      && Itv.le a1.null_pos a2.null_pos
      && PowStruct.le a1.structure a2.structure

  let resize orig_st new_st x = Itv.divide (Itv.times x orig_st) new_st
  let cast typ arr =
    match typ with
      Cil.TPtr ((Cil.TComp (comp, _) as t'), _) ->
        let new_stride = try CilHelper.byteSizeOf t' |> Itv.of_int with _ -> Itv.top in
        { offset = resize arr.stride new_stride arr.offset;
          size = resize arr.stride new_stride arr.size;
          null_pos = resize arr.stride new_stride arr.null_pos;
          stride = new_stride;
          structure = PowStruct.add comp.Cil.cname arr.structure }
    | Cil.TPtr (t', _) | Cil.TArray (t', _, _) ->
        let new_stride = try CilHelper.byteSizeOf t' |> Itv.of_int with _ -> Itv.top in
        { arr with
            offset = resize arr.stride new_stride arr.offset;
            size = resize arr.stride new_stride arr.size;
            null_pos = resize arr.stride new_stride arr.null_pos;
            stride = new_stride; }
    | _ -> arr

  let weak_plus_size arr i = { arr with size = Itv.join arr.size (Itv.plus i arr.size) }
  let plus_offset arr i = { arr with offset = Itv.plus arr.offset i }
  let minus_offset arr i = { arr with offset = Itv.minus arr.offset i }
  let set_null_pos arr i = { arr with null_pos = i }
  let plus_null_pos arr i = { arr with null_pos = Itv.plus arr.null_pos i }

  let to_string arr =
    "("^(Itv.to_string arr.offset)^", "^(Itv.to_string arr.size)^", "^(Itv.to_string arr.stride)
    ^", "^(Itv.to_string arr.null_pos)^", "^(PowStruct.to_string arr.structure)^")"

  let pp fmt arr =
    Format.fprintf fmt "@[<hov 2>( %a, %a, %a, %a, %a )@]" Itv.pp arr.offset
      Itv.pp arr.size Itv.pp arr.stride Itv.pp arr.null_pos PowStruct.pp arr.structure
end

include MapDom.MakeLAT (Allocsite) (ArrInfo)

let make : Allocsite.t -> Itv.t -> Itv.t -> Itv.t -> Itv.t -> t
= fun a o sz st np ->
  add a (ArrInfo.make (o, sz, st, np, PowStruct.bot)) bot

let offsetof : t -> Itv.t
= fun a ->
  fold (fun _ arr -> Itv.join arr.ArrInfo.offset) a Itv.bot

let sizeof : t -> Itv.t
= fun a ->
  fold (fun _ arr -> Itv.join arr.ArrInfo.size) a Itv.bot

let nullof : t -> Itv.t
= fun a ->
  fold (fun _ arr -> Itv.join arr.ArrInfo.null_pos) a Itv.bot

let extern allocsite =
  if !Options.top_location then top
  else add allocsite ArrInfo.top empty

let input allocsite =
  if !Options.top_location then top
  else add allocsite ArrInfo.input empty

let weak_plus_size : t -> Itv.t -> t
= fun arr i ->
  map (fun a -> ArrInfo.weak_plus_size a i) arr

let plus_offset : t -> Itv.t -> t
= fun arr i ->
  map (fun a -> ArrInfo.plus_offset a i) arr

let minus_offset : t -> Itv.t -> t
= fun arr i ->
  map (fun a -> ArrInfo.minus_offset a i) arr

let set_null_pos : t -> Itv.t -> t
= fun arr i ->
  map (fun a -> ArrInfo.set_null_pos a i) arr

let plus_null_pos : t -> Itv.t -> t
= fun arr i ->
  map (fun a -> ArrInfo.plus_null_pos a i) arr

let cast_array : Cil.typ -> t -> t
= fun typ a ->
  mapi (fun allocsite -> if Allocsite.is_cmd_arg allocsite then id else ArrInfo.cast typ) a

let allocsites_of_array a =
  foldi (fun k _ -> BatSet.add k) a BatSet.empty

let pow_loc_of_array : t -> PowLoc.t = fun array ->
  let pow_loc_of_allocsite k _ acc = PowLoc.add (Loc.of_allocsite k) acc in
  foldi pow_loc_of_allocsite array PowLoc.bot

let struct_of_array a =
  foldi (fun k v ->
      if PowStruct.bot <> v.ArrInfo.structure then PowLoc.add (Loc.of_allocsite k)
      else id) a PowLoc.bot

let append_field : t -> Cil.fieldinfo -> PowLoc.t
= fun s f ->
  foldi (fun a info ->
      if PowStruct.mem f.Cil.fcomp.Cil.cname info.ArrInfo.structure then
        let loc = Loc.of_allocsite a in
        PowLoc.add (Loc.append_field loc f.Cil.fname f.Cil.ftype)
      else id) s PowLoc.bot

let to_string : t -> string = fun x ->
  if is_empty x then "bot" else
  foldi (fun a b s ->
      let str = A.to_string a ^ " -> " ^ B.to_string b in
      link_by_sep "\n\t" str s) x ""

