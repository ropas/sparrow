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
(* Compile time option for profiler *)
let record_calling_frequency = false
let record_max_time = false

let events = ref BatMap.empty
let log = ref BatMap.empty
let count_log = ref BatMap.empty
let recursive_log = ref BatMap.empty
let max_log = ref BatMap.empty
let arguments_log = ref BatMap.empty
let log_start = ref 0.0

(* (internal procedure) *)
let incr_recursive : string -> int =
  fun name ->
    let v =
      if (BatMap.mem name !recursive_log) then
        (BatMap.find name !recursive_log)
      else
        let v = ref 0 in
          recursive_log := BatMap.add name v !recursive_log;
          v
    in
      v := !v + 1;
      !v

(* (internal procedure) *)
let decr_recursive : string -> int =
  fun name ->
    let v = BatMap.find name !recursive_log in
      v := !v - 1;
      !v

(* (internal procedure) *)
let record_arguments : string -> 'a -> unit =
  fun name args ->
    let v =
      if BatMap.mem name !arguments_log then
        BatMap.find name !arguments_log
      else
        let v = ref "" in
          arguments_log := (BatMap.add name v !arguments_log);v
    in
      v := (Marshal.to_string args [Marshal.No_sharing;Marshal.Closures])

(* (internal procedure) *)
let max_check : string -> float -> 'a -> unit =
  fun name t args ->
    let v = 
      if (BatMap.mem name !max_log) then
        (BatMap.find name !max_log)
      else
        let v = ref 0. in
          max_log := BatMap.add name v !max_log;v
    in
      if !v < t then
        begin
          (* record_arguments name args; *)
          v := t
        end
      
let max_check =
  if record_max_time then max_check
  else (fun _ _ _ -> ())

(* (internal procedure) *)
let count_one : string -> unit =
  fun name ->
    if (BatMap.mem name !count_log) then
      (BatMap.find name !count_log) := (Int64.succ !(BatMap.find name !count_log))
    else
      count_log := BatMap.add name (ref Int64.one) !count_log

let count_one =
  if record_calling_frequency then count_one
  else (fun _ -> ())

(* (internal procedure) *)
let update_log : string -> float -> unit =
  fun name interval ->
    let v =
      if BatMap.mem name !log then
        BatMap.find name !log
      else
        let v = ref 0. in
          log := BatMap.add name v !log;v
    in
      v := !v +. interval

let start_logger : unit -> unit =
  fun () ->
    log_start := Sys.time ()

let start_event : string -> unit =
  fun name ->
    if !Options.profile then
      begin
        let st = Sys.time() in
          if (incr_recursive name) == 1 then
            events := BatMap.add name st !events
      end
    else
      ()

let finish_event : string -> unit =
  fun name ->
    if !Options.profile then
      begin
        let ft = Sys.time() in
          count_one name;
          let v = decr_recursive name in
            if v == 0 then
              let t0 = BatMap.find name !events in
              let el = (ft -. t0) in
                events := BatMap.remove name !events;
                update_log name el
      end
    else
      ()
let event : string -> ('a -> 'b) -> 'a -> 'b
= fun name f x ->
  start_event name;
  let y = f x in
  finish_event name;
  y

let make_filename : unit -> string =
  fun () ->
    let name : int -> string =
      fun i -> "marshal_"^(string_of_int i)^".out"
    in
    let rec make_file_i : int -> string =
      fun i ->
        if Sys.file_exists (name i) then make_file_i (i+1)
        else (name i)
    in
      make_file_i 0

let make_file : string -> string =
  fun txt ->
    let name = make_filename () in
    let fileout = open_out_bin name in
      output_string fileout txt;
      close_out fileout;
      name

let report : out_channel -> unit =
  fun c ->
    if !Options.profile then
      begin
        let tot = Sys.time() -. !log_start in
          begin
(*            print_endline("------- Profiler report -------"); *)
            Printf.fprintf c " - Total time";
            Printf.fprintf c "  %.2fs\n" tot;
(*            print_endline(" - Elapsed time"); *)
            BatMap.iter (fun x v -> Printf.fprintf c "  -%s - %.2fs(%.2f%%)\n" x !v (!v /. tot *. 100.)) !log;
(*            print_endline(" -------------------------------"); *)
(*            print_endline("- Calling frequency");
            BatMap.iter (fun x v -> Printf.fprintf c "2. %s - %s\n" x (Int64.to_string !v)) !count_log;
            print_endline("-------------------------------");
            print_endline("- Maximum elapsed time for one event");
            BatMap.iter (fun x v -> Printf.fprintf c "3. %s - %.2fs\n" x !v) !max_log;
            print_endline("-------------------------------");
            print_endline("- Marshaled datas");
            BatMap.iter (fun x v -> Printf.fprintf c "4. %s - %s\n" x (make_file !v)) !arguments_log;
            print_endline("-------------------------------");
*)          end
      end
    else
      ()
		
let reset () = log := BatMap.empty; log_start := Sys.time()
