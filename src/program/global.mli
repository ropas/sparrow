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
(** Global information *)

type t = {
  file : Cil.file;
  icfg : InterCfg.t;
  callgraph : CallGraph.t;
  dump : BasicDom.Dump.t;
  mem : ItvDom.Mem.t;
  table : ItvDom.Table.t;
}

val init : Cil.file -> t

val is_rec : InterCfg.pid -> t -> bool
val is_undef : InterCfg.pid -> t -> bool

val remove_unreachable_functions : t -> t
