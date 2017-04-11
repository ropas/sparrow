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

val opt_il : bool ref
val opt_cfg : bool ref
val opt_dug : bool ref
val opt_optil : bool ref

(** {2 Context Sensitivity } *)

val opt_inline : string list ref
val opt_inline_size : int ref
val opt_pfs : int ref
val opt_pfs_wv : string ref

(** {2 Octagon Analysis } *)

val opt_oct : bool ref
val opt_pack_impact : bool ref
val opt_pack_manual : bool ref

(** {2 Unsoundness } *)

val opt_unsound_loop : string BatSet.t ref
val opt_unsound_lib : string BatSet.t ref
val opt_extract_loop_feat : bool ref
val opt_extract_lib_feat : bool ref
val opt_top_location : bool ref
val opt_bugfinder :  int ref

(** {2 Main Analysis } *)

val opt_narrow : bool ref
val opt_scaffold : bool ref
val opt_int_overflow : bool ref

(** {2 Alarm Report } *)

val opt_bo : bool ref
val opt_nd : bool ref
val opt_dz : bool ref
val opt_show_all_query : bool ref

(** {2 Pretty Printer & Debugging } *)

val opt_nobar : bool ref
val opt_profile : bool ref
val opt_noalarm : bool ref
val opt_debug : bool ref
val opt_oct_debug : bool ref
val opt_print_premem : bool ref
val opt_verbose : int ref


(** {2 Marshaling } *)

val opt_marshal_in : bool ref
val opt_marshal_out : bool ref
val opt_marshal_dir : string ref

(** {2 Options lists } *)

val opts : (string * Arg.spec * string) list
