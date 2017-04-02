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
(** Profiler *)
val record_calling_frequency : bool
val record_max_time : bool
val events : (string, float) BatMap.t Pervasives.ref
val log : (string, float Pervasives.ref) BatMap.t Pervasives.ref
val count_log : (string, int64 Pervasives.ref) BatMap.t Pervasives.ref
val recursive_log : (string, int Pervasives.ref) BatMap.t Pervasives.ref
val max_log : (string, float Pervasives.ref) BatMap.t Pervasives.ref
val arguments_log : (string, string Pervasives.ref) BatMap.t Pervasives.ref
val log_start : float Pervasives.ref
val incr_recursive : string -> int
val decr_recursive : string -> int
val record_arguments : string -> 'a -> unit
val max_check : string -> float -> 'a -> unit
val max_check : string -> float -> 'a -> unit
val count_one : string -> unit
val count_one : string -> unit
val update_log : string -> float -> unit
val start_logger : unit -> unit
val event : string -> ('a -> 'b) -> 'a -> 'b
val start_event : string -> unit
val finish_event : string -> unit
val make_filename : unit -> string
val make_file : string -> string
val report : Pervasives.out_channel -> unit
val reset : unit -> unit
