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
(** Alarm report of interval analysis *)
type target = BO | ND | DZ
type status = Proven | UnProven | BotAlarm
type query = {
  node : InterCfg.node;
  exp : AlarmExp.t;
  loc : Cil.location;
  allocsite : BasicDom.Allocsite.t option;
  status : status;
  desc : string
}
type part_unit = Cil.location
val sort_partition : (part_unit * query list) list -> (part_unit * query list) list
val string_of_alarminfo : Itv.t -> Itv.t -> string
val string_of_query : query -> string
val partition : query list -> (part_unit, query list) BatMap.t
val get : query list -> status -> query list
val print : query list -> unit
