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
(** Vocabularies *)

val ( <<< ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val ( >>> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val ($>)  : 'a option -> ('a -> 'b option) -> 'b option
val (&>)  : 'a option -> ('a -> 'b) -> 'b option
val ( @ ) : 'a list -> 'a list -> 'a list
val id : 'a -> 'a
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val cond : bool -> ('a -> 'b) -> ('a -> 'b) -> 'a -> 'b
val opt : bool -> ('a -> 'a) -> 'a -> 'a
val case : (bool * ('a -> 'b)) list -> ('a -> 'b) -> 'a -> 'b
val tuple : 'a -> 'a * 'a

val compare_string : string -> string -> int
val compare_bool : bool -> bool -> int
val compare_int : int -> int -> int

val domof : ('a, 'b) BatMap.t -> 'a BatSet.t
val list_fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
val list_fold2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
val list_rev : 'a list -> 'a list
val append_opt : 'a option -> 'a list -> 'a list
val find_opt : 'a -> ('a, 'b) BatMap.t -> 'b option
val find_def : 'a -> ('a, 'b) BatMap.t -> 'b -> 'b
val link_by_sep : bytes -> bytes -> bytes -> bytes
val string_of_list :
  ?first:bytes ->
  ?last:bytes -> ?sep:bytes -> ('a -> bytes) -> 'a list -> bytes
val string_of_set :
  ?first:bytes ->
  ?last:bytes -> ?sep:bytes -> ('a -> bytes) -> 'a BatSet.t -> bytes
val string_of_map :
  ?first:bytes ->
  ?last:bytes ->
  ?sep:bytes ->
  ?indent:bytes ->
  ('a -> bytes) ->
  ('b -> bytes) -> ('a, 'b) BatMap.t -> bytes
val i2s : int -> bytes
val list2set : 'a list -> 'a BatSet.t
val set2list : 'a BatSet.t -> 'a list
val set_union_small_big : 'a BatSet.t -> 'a BatSet.t -> 'a BatSet.t
val prerr_progressbar : ?itv:int -> int -> int -> unit
val fix : ('a BatSet.t -> 'a BatSet.t) -> 'a BatSet.t -> 'a BatSet.t
val my_prerr_endline : bytes -> unit
val my_prerr_newline : unit -> unit
val my_prerr_string : bytes -> unit
val prerr_memory_usage : unit -> unit
