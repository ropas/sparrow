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
(** Commandline options *)

(** {2 Intermediate Represenation } *)

val il : bool ref
val cfg : bool ref
val dug : bool ref
val optil : bool ref

(** {2 Context Sensitivity } *)

val inline : string list ref
val inline_size : int ref
val pfs : int ref
val pfs_wv : string ref

(** {2 Octagon Analysis } *)

val oct : bool ref
val pack_impact : bool ref
val pack_manual : bool ref

(** {2 Unsoundness } *)

val unsound_loop : string BatSet.t ref
val unsound_lib : string BatSet.t ref
val extract_loop_feat : bool ref
val extract_lib_feat : bool ref
val top_location : bool ref
val bugfinder :  int ref
val unsound_recursion : bool ref
val unsound_alloc : bool ref

(** {2 Main Analysis } *)

val narrow : bool ref
val scaffold : bool ref
val int_overflow : bool ref

(** {2 Alarm Report } *)

val bo : bool ref
val nd : bool ref
val dz : bool ref
val show_all_query : bool ref

(** {2 Pretty Printer & Debugging } *)

val nobar : bool ref
val profile : bool ref
val noalarm : bool ref
val debug : bool ref
val oct_debug : bool ref
val print_premem : bool ref
val verbose : int ref


(** {2 Marshaling } *)

val marshal_in : bool ref
val marshal_out : bool ref
val marshal_dir : string ref

(** {2 Options lists } *)

val opts : (string * Arg.spec * string) list
