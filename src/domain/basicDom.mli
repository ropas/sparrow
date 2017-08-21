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
(** Basic abstract domains *)
open AbsDom

module Node = InterCfg.Node
module PowNode : PowDom.CPO with type t = PowDom.MakeCPO(Node).t and type elt = Node.t
module Proc = InterCfg.Proc
module PowProc : PowDom.CPO with type t = PowDom.MakeCPO(Proc).t and type elt = Proc.t

module Allocsite : sig
  include AbsDom.SET
  val allocsite_of_node : Node.t -> t
  val allocsite_of_string : Node.t -> t
  val is_string_allocsite : t -> bool
  val is_ext_allocsite : t -> bool
  val is_cmd_arg : t -> bool
  val allocsite_of_ext : string option -> t
end

module Loc : sig
  type t = GVar of string * Cil.typ | LVar of Proc.t * string * Cil.typ | Allocsite of Allocsite.t
  | Field of t * field * Cil.typ
  and field = string
  include AbsDom.SET with type t := t
  val null : t
  val dummy : t
  val is_var : t -> bool
  val is_lvar : t -> bool
  val is_gvar : t -> bool
  val is_allocsite : t -> bool
  val is_ext_allocsite : t -> bool
  val is_field : t -> bool
  val of_gvar : string -> Cil.typ -> t
  val of_lvar : Proc.t -> string -> Cil.typ -> t
  val append_field : t -> field -> Cil.typ -> t
  val of_allocsite : Allocsite.t -> t
  val return_var : Proc.t -> Cil.typ -> t
  val is_local_of : Proc.t -> t -> bool
  val get_proc : t -> Proc.t
  val typ : t -> Cil.typ option
end

module PowLoc : sig
  include PowDom.CPO
  val null : t
  val prune : Cil.binop -> t -> Cil.exp -> t
  val append_field : t -> Cil.fieldinfo -> t
end with type t = PowDom.MakeCPO(Loc).t and type elt = Loc.t

module Dump : MapDom.CPO with type A.t = Proc.t and type B.t = PowLoc.t
