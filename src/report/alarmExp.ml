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
open Cil
open IntraCfg
open Cmd

type t =
  | ArrayExp of lval * exp * location
  | DerefExp of exp * location
  | DivExp of exp * exp * location
  | Strcpy of exp * exp * location
  | Strcat of exp * exp * location
  | Strncpy of exp * exp * exp * location
  | Memcpy of exp * exp * exp * location
  | Memmove of exp * exp * exp * location

let to_string t =
  match t with
  | ArrayExp (lv,e,_) -> CilHelper.s_lv lv ^ "[" ^ CilHelper.s_exp e ^ "]"
  | DerefExp (e,_) -> "*(" ^ CilHelper.s_exp e ^ ")"
  | DivExp (e1, e2, _) -> CilHelper.s_exp e1 ^ " / " ^ CilHelper.s_exp e2
  | Strcpy (e1, e2, _) -> "strcpy ("^(CilHelper.s_exp e1)^", "^(CilHelper.s_exp e2)^")"
  | Strncpy (e1, e2, e3, _) -> "strncpy ("^(CilHelper.s_exp e1)^", "^(CilHelper.s_exp e2)^", "^(CilHelper.s_exp e3)^")"
  | Memcpy (e1, e2, e3, _) -> "memcpy ("^(CilHelper.s_exp e1)^", "^(CilHelper.s_exp e2)^", "^(CilHelper.s_exp e3)^")"
  | Memmove (e1, e2, e3, _) -> "memmove ("^(CilHelper.s_exp e1)^", "^(CilHelper.s_exp e2)^", "^(CilHelper.s_exp e3)^")"
  | Strcat (e1, e2, _) -> "strcat ("^(CilHelper.s_exp e1)^", "^(CilHelper.s_exp e2)^")"

let location_of = function
  | ArrayExp (_,_,l)
  | DerefExp (_,l)
  | DivExp (_, _, l)
  | Strcpy (_, _, l)
  | Strncpy (_, _, _, l)
  | Memcpy (_, _, _, l)
  | Memmove (_, _, _, l)
  | Strcat (_, _, l) -> l

(* NOTE: you may use Cil.addOffset or Cil.addOffsetLval instead of
   add_offset, append_field, and append_index. *)
let rec add_offset : Cil.offset -> Cil.offset -> Cil.offset
=fun o orig_offset ->
  match orig_offset with
  | NoOffset -> o
  | Field (f, o1) -> Field (f, add_offset o o1)
  | Index (e, o1) -> Index (e, add_offset o o1)

let append_field : Cil.lval -> Cil.fieldinfo -> Cil.lval
=fun lv f -> (fst lv, add_offset (Field (f, NoOffset)) (snd lv))
let append_index : Cil.lval -> Cil.exp -> Cil.lval
=fun lv e -> (fst lv, add_offset (Index (e, NoOffset)) (snd lv))

let rec c_offset : Cil.lval -> Cil.offset -> Cil.location -> t list
=fun lv offset loc ->
  match offset with
  | NoOffset -> []
  | Field (f,o) -> c_offset (append_field lv f) o loc
  | Index (e,o) ->
    (ArrayExp (lv, e, loc)) :: (c_exp e loc)
    @ (c_offset (append_index lv e) o loc)

and c_lv : Cil.lval -> Cil.location -> t list
=fun lv loc ->
  match lv with
  | Var v, offset   -> c_offset (Var v, NoOffset) offset loc
  | Mem exp, offset ->
    (DerefExp (exp, loc)) :: (c_exp exp loc)
    @ (c_offset (Mem exp, NoOffset) offset loc)

and c_exp : Cil.exp -> Cil.location -> t list
=fun e loc ->
  match e with
  | Lval lv -> c_lv lv loc
  | AlignOfE e -> c_exp e loc
  | UnOp (_,e,_) -> c_exp e loc
  | BinOp (bop,e1,e2,_) ->
    begin
      match bop with
        Div | Mod -> DivExp (e1, e2, loc) :: (c_exp e1 loc) @ (c_exp e2 loc)
      | _ -> (c_exp e1 loc) @ (c_exp e2 loc)
    end
  | CastE (_,e) -> c_exp e loc
  | AddrOf lv -> c_lv lv loc
  | StartOf lv -> c_lv lv loc
  | _ -> []

and c_exps exps loc = List.fold_left (fun q e -> q @ (c_exp e loc)) [] exps

let query_lib = ["strcpy"; "memcpy"; "memmove"; "strncpy"; "strcat"]

let c_lib f es loc =
  match f.vname with
  | "strcpy" -> (Strcpy (List.nth es 0, List.nth es 1, loc)) :: (c_exps es loc)
  | "memcpy" -> (Memcpy (List.nth es 0, List.nth es 1, List.nth es 2, loc))::(c_exps es loc)
  | "memmove" -> (Memmove (List.nth es 0, List.nth es 1, List.nth es 2, loc))::(c_exps es loc)
  | "strncpy" -> (Strncpy (List.nth es 0, List.nth es 1, List.nth es 2, loc))::(c_exps es loc)
  | "strcat" -> (Strcat (List.nth es 0, List.nth es 1, loc)) :: (c_exps es loc)
  | _ -> []

let rec collect : IntraCfg.cmd -> t list
=fun cmd ->
  match cmd with
  | Cmd.Cset (lv,e,loc) -> (c_lv lv loc) @ (c_exp e loc)
  | Cmd.Cexternal (lv,loc) -> c_lv lv loc
  | Cmd.Calloc (lv,Array e,_,loc) -> (c_lv lv loc) @ (c_exp e loc)
  | Cmd.Csalloc (lv,_,loc) -> c_lv lv loc
  | Cmd.Cassume (e,loc) -> c_exp e loc
  | Cmd.Creturn (Some e, loc) -> c_exp e loc
  | Cmd.Ccall (_, Lval (Var f, NoOffset), es, loc) when List.mem f.vname query_lib -> c_lib f es loc
  | Cmd.Ccall (None,e,es,loc) -> (c_exp e loc) @ (c_exps es loc)
  | Cmd.Ccall (Some lv,e,es,loc) -> (c_lv lv loc) @ (c_exp e loc) @ (c_exps es loc)
  | _ -> []
