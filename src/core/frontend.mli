(***********************************************************************)
(* Copyright (c) 2007-present.                                         *)
(* Programming Research Laboratory (ROPAS), Seoul National University. *)
(* All rights reserved.                                                *)
(*                                                                     *)
(* This software is distributed under the term of the BSD license.     *)
(* See the LICENSE file for details.                                   *)
(*                                                                     *)
(***********************************************************************)
(** Frontend *)
open Cil

val files : string list ref
val marshal_file : string ref
val args : string -> unit
val parse : unit -> Cil.file
val makeCFGinfo : Cil.file -> Cil.file
val is_varargs : string -> Cil.file -> bool
val inline : Global.t -> bool
