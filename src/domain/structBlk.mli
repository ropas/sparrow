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
(** Struct domain *)
module Struct : AbsDom.SET with type t = string
module PowStruct : PowDom.S with type elt = Struct.t
include MapDom.S with type A.t = BasicDom.Loc.t and type B.t = PowStruct.t
val make : BasicDom.PowLoc.t -> Cil.compinfo -> t
val append_field : t -> Cil.fieldinfo -> BasicDom.PowLoc.t 
val pow_loc_of_struct : t -> BasicDom.PowLoc.t
val to_string : t -> string
