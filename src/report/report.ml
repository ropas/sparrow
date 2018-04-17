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
open Vocab
open BasicDom
open ItvDom
open Cil
open IntraCfg
open Cmd
open AlarmExp

type target = BO | ND | DZ
type status = Proven | UnProven | BotAlarm
type part_unit = Cil.location

let status_to_string = function Proven -> "Proven" | UnProven -> "UnProven" | _ -> "BotAlarm"

type query = {
  node : InterCfg.node;
  exp : AlarmExp.t;
  loc : Cil.location;
  allocsite : Allocsite.t option;
  status : status;
  desc : string
}

let is_unproven : query -> bool
=fun q -> q.status = UnProven

let get_pid : query -> string
=fun q -> InterCfg.Node.get_pid q.node

let get qs status =
  List.filter (fun q -> q.status = status) qs

let string_of_alarminfo offset size =
  "offset: " ^ Itv.to_string offset ^ ", size: " ^ Itv.to_string size

let partition : query list -> (part_unit, query list) BatMap.t
=fun queries ->
  list_fold (fun q m ->
    let p_als = try BatMap.find q.loc m with _ -> [] in
      BatMap.add q.loc (q::p_als) m
  ) queries BatMap.empty

let sort_queries : query list -> query list =
fun queries ->
  List.sort (fun a b ->
    if Pervasives.compare a.loc.file b.loc.file = 0 then
    begin
      if Pervasives.compare a.loc.line b.loc.line = 0 then
        Pervasives.compare a.exp b.exp
      else Pervasives.compare a.loc.line b.loc.line
    end
    else Pervasives.compare a.loc.file b.loc.file) queries

let sort_partition : (part_unit * query list) list -> (part_unit * query list) list =
fun queries ->
  List.sort (fun (a,_) (b,_) ->
    if Pervasives.compare a.file b.file = 0 then
      Pervasives.compare a.line b.line
    else Pervasives.compare a.file b.file) queries

let get_status : query list -> status
=fun queries ->
  if List.exists (fun q -> q.status = BotAlarm) queries then BotAlarm
  else if List.exists (fun q -> q.status = UnProven) queries then  UnProven
  else if List.for_all (fun q -> q.status = Proven) queries then Proven
  else raise (Failure "Report.ml: get_status")

let get_proved_query_point : query list -> part_unit BatSet.t
=fun queries ->
  let all = partition queries in
  let unproved = partition (get queries UnProven) in
  let all_loc = BatMap.foldi (fun l _ -> BatSet.add l) all BatSet.empty in
  let unproved_loc = BatMap.foldi (fun l _ -> BatSet.add l) unproved BatSet.empty in
    BatSet.diff all_loc unproved_loc

let string_of_query q =
  (CilHelper.s_location q.loc)^ " "^
  (AlarmExp.to_string q.exp) ^ " @" ^
  (InterCfg.Node.to_string q.node) ^ ":  " ^
  (match q.allocsite with
    Some a -> Allocsite.to_string a
   | _ -> "") ^ "  " ^
  q.desc ^ " " ^ status_to_string (get_status [q])

let filter_extern partition =
  BatMap.filterv (fun ql ->
      not (List.exists (fun q ->
          match q.allocsite with
          | Some allocsite -> Allocsite.is_ext_allocsite allocsite
          | None -> false) ql)) partition

let filter_global partition =
  BatMap.filterv (fun ql ->
      not (List.exists (fun q ->
          InterCfg.Node.get_pid q.node = InterCfg.global_proc) ql)) partition

let filter_lib partition =
  BatMap.filterv (fun ql ->
      not (List.exists (fun q ->
          match q.exp with
          | AlarmExp.Strcpy (_, _, _) | AlarmExp.Strcat (_, _, _)
          | AlarmExp.Strncpy (_, _, _, _) | AlarmExp.Memcpy (_, _, _, _)
          | AlarmExp.Memmove (_, _, _, _) -> true
          | _ -> false) ql)) partition

let filter_rec global partition =
  BatMap.filterv (fun ql ->
      not (List.exists (fun q ->
          Global.is_rec (InterCfg.Node.get_pid q.node) global) ql)) partition

let filter_complex_exp partition =
  BatMap.filterv (fun ql ->
      not (List.exists (fun q ->
        match q.exp with
        | AlarmExp.ArrayExp (_, BinOp (bop, _, _, _), _) -> bop = BAnd || bop = BOr || bop = BXor
        | AlarmExp.ArrayExp (_, Lval (Var vi, _), _) -> vi.vglob
        | AlarmExp.DerefExp (Lval (Var vi, _), _) -> vi.vglob
        | _ -> false) ql)) partition

let filter_allocsite partition =
  BatMap.filterv (fun ql ->
      not (List.exists (fun q ->
          match q.allocsite with
          | Some allocsite -> BatSet.mem (Allocsite.to_string allocsite) !Options.filter_allocsite
          | None -> false) ql)) partition

let alarm_filter global part =
  part
  |> opt !Options.filter_extern filter_extern
  |> opt !Options.filter_global filter_global
  |> opt !Options.filter_lib filter_lib
  |> opt !Options.filter_complex_exp filter_complex_exp
  |> opt !Options.filter_rec (filter_rec global)
  |> opt (not (BatSet.is_empty !Options.filter_allocsite)) filter_allocsite

let display_alarms title alarms_part =
  prerr_endline "";
  prerr_endline ("= " ^ title ^ " =");
  let alarms_part = BatMap.bindings alarms_part in
  let alarms_part = sort_partition alarms_part in
  List.iteri (fun k (part_unit, qs) ->
    prerr_string (string_of_int (k + 1) ^ ". " ^ CilHelper.s_location part_unit ^ " ");
    prerr_string (string_of_set id (list2set (List.map (fun q -> InterCfg.Node.get_pid q.node) qs)));
    prerr_string (" " ^ status_to_string (get_status qs));
    prerr_newline ();
    List.iter (fun q ->
      prerr_string ( "  " ^ AlarmExp.to_string q.exp ^ " @");
      prerr_string (InterCfg.Node.to_string q.node);
      prerr_string ( ":  " ^ q.desc ^ " " ^ status_to_string (get_status [q]));
      (match q.allocsite with Some a ->
        prerr_endline ( ", allocsite: " ^ Allocsite.to_string a)
       | _ -> prerr_newline ())
    ) qs
  ) alarms_part

let print global queries =
  let all = partition queries in
  let unproven = partition (get queries UnProven) |> alarm_filter global in
  let bot = partition (get queries BotAlarm) in
  if not !Options.noalarm then
    begin
      display_alarms "Alarms" (if !Options.show_all_query then all else unproven);
    end
  else ();
  prerr_endline "";
  prerr_endline ("#queries                 : " ^ i2s (List.length queries));
  prerr_endline ("#queries mod alarm point : " ^ i2s (BatMap.cardinal all));
  prerr_endline ("#proven                  : " ^ i2s (BatSet.cardinal (get_proved_query_point queries)));
  prerr_endline ("#unproven                : " ^ i2s (BatMap.cardinal unproven));
  prerr_endline ("#bot-involved            : " ^ i2s (BatMap.cardinal bot))

let print_raw : bool -> query list -> unit
=fun summary_only queries ->
  let unproven = List.filter (fun x -> x.status = UnProven) queries in
  let botalarm = List.filter (fun x -> x.status = BotAlarm) queries in
  prerr_newline ();
  prerr_endline ("= "^"Alarms"^ "=");
  List.iteri (fun k q ->
    prerr_string (string_of_int (k + 1) ^ ". ");
    prerr_string ( "  " ^ AlarmExp.to_string q.exp ^ " @");
    prerr_string (InterCfg.Node.to_string q.node);
    prerr_string ("  ");
    prerr_string (CilHelper.s_location q.loc);
    prerr_endline ( ":  " ^ q.desc )
  ) (sort_queries unproven);
  prerr_endline "";
  prerr_endline ("#queries                 : " ^ i2s (List.length queries));
  prerr_endline ("#proven                  : " ^ i2s (BatSet.cardinal (get_proved_query_point queries)));
  prerr_endline ("#unproven                : " ^ i2s (List.length unproven));
  prerr_endline ("#bot-involved            : " ^ i2s (List.length botalarm))
