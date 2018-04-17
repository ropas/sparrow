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
open Arg

(* IL *)
let il = ref false
let cfg = ref false
let dug = ref false
let optil = ref true

(* Context & Flow Sensitivity *)
let inline = ref []
let inline_size = ref 100000
let pfs = ref 100
let pfs_wv = ref
  "100.0 -100.0 -34.988241686898462 -99.662940999334793 -100.0
   20.989776538253508 100.0 28.513793013882694 -30.30168857094921 -100.0
   27.574102626204336 -74.381386730147895 -65.270452404579814 100.0 100.0
   80.703727126015522 -13.475885118852554 -100.0 67.910455368871894 -100.0
   -100.0 -58.103715871839746 -100.0 100.0 -2.1779606787169481
   50.854271919870321 87.790748577623447 100.0 100.0 -100.0
   -100.0 100.0 70.038390871188767 -22.179572133292666 100.0
   42.647897970881758 100.0 -100.0 -100.0 32.564292030707847
   98.370519929542738 100.0 100.0 -100.0 -100.0"

(* Octagon Analysis *)
let oct = ref false
let pack_impact = ref true
let pack_manual = ref false

(* Analyzer *)
let nobar = ref false
let narrow = ref false
let profile = ref false
let scaffold = ref true

(* Unsoundness *)
let unsound_loop = ref BatSet.empty
let unsound_lib = ref BatSet.empty
let extract_loop_feat = ref false
let extract_lib_feat = ref false
let top_location = ref false
let unsound_recursion = ref false
let unsound_alloc = ref false
let bugfinder = ref 0

(* Alarm Report *)
let noalarm = ref false
let bo = ref true
let nd = ref false
let dz = ref false
let show_all_query = ref false
let filter_extern = ref false
let filter_global = ref false
let filter_lib = ref false
let filter_complex_exp = ref false
let filter_rec = ref false
let filter_allocsite = ref BatSet.empty

(* Marshaling *)
let marshal_in = ref false
let marshal_out = ref false
let marshal_dir = ref "marshal"

(* Debug *)
let debug = ref false
let oct_debug = ref false

(* ETC *)
let print_premem = ref false
let verbose = ref 1
let int_overflow = ref false

let opts =
  [
  ("-il", (Arg.Set il), "Show the input program in IL");
  ("-cfg", (Arg.Set cfg), "Print Cfg");
  ("-dug", (Arg.Set dug), "Print Def-Use graph");
  ("-noalarm", (Arg.Set noalarm), "Do not print alarms");
  ("-verbose", (Arg.Int (fun x -> verbose := x)), "Verbose level (default: 1)");
  ("-debug", (Arg.Set debug), "Print debug information");
  ("-oct_debug", (Arg.Set oct_debug), "Print debug information for octagon analysis");
  ("-pack_impact", (Arg.Set pack_impact), "Packing by impact pre-analysis");
  ("-pack_manual", (Arg.Set pack_manual), "Pacing by manual annotation");
  ("-nd", (Arg.Set nd), "Print Null-dereference alarms");
  ("-bo", (Arg.Set bo), "Print Buffer-overrun alarms");
  ("-dz", (Arg.Set dz), "Print Divide-by-zero alarms");
  ("-bugfinder", (Arg.Int (fun x -> bugfinder := x)), "Unsoundness level in bugfinding mode (default: 0)");
  ("-inline", (Arg.String (fun s -> inline := s::(!inline))), "Inline functions whose names contain X");
  ("-inline_size", (Arg.Int (fun x -> inline_size := x)), "Size constraint for function inline");
  ("-pfs", (Arg.Int (fun x -> pfs := x)), "Partial flow-sensitivity -pfs [0-100] (0: flow-insensitive, 100: fully flow-sensitive). default=100");
  ("-pfs_wv", (Arg.String (fun s -> pfs_wv := s)), "Weight vector for flow-sensitivity (e.g., \"0 1 -1 ... \"). Unspecified weights are zeros.");
  ("-oct", (Arg.Set oct), "Do octagon analysis");
  ("-profile", (Arg.Set profile), "Profiler");
  ("-narrow", (Arg.Set narrow), "Do narrowing");
  ("-unsound_loop", (Arg.String (fun s -> unsound_loop := BatSet.add s !unsound_loop)), "Unsound loops");
  ("-unsound_lib", (Arg.String (fun s -> unsound_lib := BatSet.add s !unsound_lib)), "Unsound libs");
  ("-unsound_recursion", (Arg.Set unsound_recursion), "Unsound recursive calls");
  ("-unsound_alloc", (Arg.Set unsound_alloc), "Unsound memory allocation (never return null)");
  ("-extract_loop_feat", (Arg.Set extract_loop_feat), "Extract features of loops for harmless unsoundness");
  ("-extract_lib_feat", (Arg.Set extract_lib_feat), "Extract features of libs for harmless unsoundness");
  ("-top_location", (Arg.Set top_location), "Treat unknown locations as top locations");
  ("-scaffold", (Arg.Set scaffold), "Use scaffolding semantics (default)");
  ("-no_scaffold", (Arg.Clear scaffold), "Do not use scaffolding semantics");
  ("-nobar", (Arg.Set nobar), "No progress bar");
  ("-show_all_query", (Arg.Set show_all_query), "Show all queries");
  ("-filter_alarm", (Arg.Unit (fun () ->
       filter_complex_exp := true;
       filter_extern := true;
       filter_global := true;
       filter_lib := true;
       filter_rec := true)), "Trun on all the filtering options");
  ("-filter_allocsite", (Arg.String (fun s -> filter_allocsite := BatSet.add s !filter_allocsite)), "Filter alarms from a given allocsite");
  ("-filter_complex_exp", (Arg.Set filter_complex_exp), "Filter alarms from complex expressions (e.g., bitwise)");
  ("-filter_extern", (Arg.Set filter_extern), "Filter alarms from external allocsites");
  ("-filter_global", (Arg.Set filter_global), "Filter alarms from the global area");
  ("-filter_lib", (Arg.Set filter_lib), "Filter alarms from library calls (e.g., strcpy)");
  ("-filter_rec", (Arg.Set filter_rec), "Filter alarms from recursive call cycles");
  ("-optil", (Arg.Set optil), "Optimize IL (default)");
  ("-no_optil", (Arg.Clear optil), "Do not optimize IL");
  ("-marshal_in", (Arg.Set marshal_in), "Read analysis results from marshaled data");
  ("-marshal_out", (Arg.Set marshal_out), "Write analysis results to marshaled data");
  ("-marshal_dir", (Arg.String (fun s -> marshal_dir := s)), "Directory where the marshaled data exists (default: marshal/)");
  ("-int_overflow", (Arg.Set int_overflow), "Consider integer overflow");
  ]
