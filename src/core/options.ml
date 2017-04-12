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
let opt_il = ref false
let opt_cfg = ref false
let opt_dug = ref false
let opt_optil = ref true

(* Context & Flow Sensitivity *)
let opt_inline = ref []
let opt_inline_size = ref 100000
let opt_pfs = ref 100
let opt_pfs_wv = ref 
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
let opt_oct = ref false
let opt_pack_impact = ref true
let opt_pack_manual = ref false

(* Analyzer *)
let opt_nobar = ref false
let opt_narrow = ref false
let opt_profile = ref false
let opt_scaffold = ref false

(* Unsoundness *)
let opt_unsound_loop = ref BatSet.empty
let opt_unsound_lib = ref BatSet.empty
let opt_extract_loop_feat = ref false
let opt_extract_lib_feat = ref false
let opt_bugfinder = ref 0

(* Alarm Report *)
let opt_noalarm = ref false
let opt_bo = ref true
let opt_nd = ref false
let opt_dz = ref false
let opt_show_all_query = ref false


(* Marshaling *)
let opt_marshal_in = ref false
let opt_marshal_out = ref false
let opt_marshal_dir = ref "marshal"

(* Debug *)
let opt_debug = ref false
let opt_oct_debug = ref false

(* ETC *)
let opt_print_premem = ref false
let opt_verbose = ref 1
let opt_int_overflow = ref false

let opts =
  [
  ("-il", (Arg.Set opt_il), "Show the input program in IL");
  ("-cfg", (Arg.Set opt_cfg), "Print Cfg");
  ("-dug", (Arg.Set opt_dug), "Print Def-Use graph");
  ("-noalarm", (Arg.Set opt_noalarm), "Do not print alarms");
  ("-verbose", (Arg.Int (fun x -> opt_verbose := x)), "Verbose level (default: 1)");
  ("-debug", (Arg.Set opt_debug), "Print debug information");
  ("-oct_debug", (Arg.Set opt_oct_debug), "Print debug information for octagon analysis");
  ("-pack_impact", (Arg.Set opt_pack_impact), "Packing by impact pre-analysis");
  ("-pack_manual", (Arg.Set opt_pack_manual), "Pacing by manual annotation");
  ("-nd", (Arg.Set opt_nd), "Print Null-dereference alarms");
  ("-bo", (Arg.Set opt_bo), "Print Buffer-overrun alarms");
  ("-dz", (Arg.Set opt_dz), "Print Divide-by-zero alarms");
  ("-bugfinder", (Arg.Int (fun x -> opt_bugfinder := x)), "Unsoundness level in bugfinding mode (default: 0)");
  ("-inline", (Arg.String (fun s -> opt_inline := s::(!opt_inline))), "Inline functions whose names contain X");
  ("-inline_size", (Arg.Int (fun x -> opt_inline_size := x)), "Size constraint for function inline");
  ("-pfs", (Arg.Int (fun x -> opt_pfs := x)), "Partial flow-sensitivity -pfs [0-100] (0: flow-insensitive, 100: fully flow-sensitive). default=100");
  ("-pfs_wv", (Arg.String (fun s -> opt_pfs_wv := s)), "Weight vector for flow-sensitivity (e.g., \"0 1 -1 ... \"). Unspecified weights are zeros.");
  ("-oct", (Arg.Set opt_oct), "Do octagon analysis");
  ("-profile", (Arg.Set opt_profile), "Profiler");
  ("-narrow", (Arg.Set opt_narrow), "Do narrowing");
  ("-unsound_loop", (Arg.String (fun s -> opt_unsound_loop := BatSet.add s !opt_unsound_loop)), "Unsound loops");
  ("-unsound_lib", (Arg.String (fun s -> opt_unsound_lib := BatSet.add s !opt_unsound_lib)), "Unsound libs");
  ("-extract_loop_feat", (Arg.Set opt_extract_loop_feat), "Extract features of loops for harmless unsoundness");
  ("-extract_lib_feat", (Arg.Set opt_extract_lib_feat), "Extract features of libs for harmless unsoundness");
  ("-scaffold", (Arg.Set opt_scaffold), "Use scaffolding semantics");
  ("-nobar", (Arg.Set opt_nobar), "No progress bar");
  ("-show_all_query", (Arg.Set opt_show_all_query), "Show all queries");
  ("-optil", (Arg.Set opt_optil), "Optimize IL");
  ("-marshal_in", (Arg.Set opt_marshal_in), "Read analysis results from marshaled data");
  ("-marshal_out", (Arg.Set opt_marshal_out), "Write analysis results to marshaled data");
  ("-marshal_dir", (Arg.String (fun s -> opt_marshal_dir := s)), "Directory where the marshaled data exists (default: marshal/)");
  ("-int_overflow", (Arg.Set opt_int_overflow), "Consider integer overflow");
  ]
