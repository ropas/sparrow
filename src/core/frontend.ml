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
open Cil
open Vocab
open Global

module C = Cil
module F = Frontc
module E = Errormsg

let files = ref []
let marshal_file = ref ""

let args : string -> unit
= fun f ->
  if Sys.file_exists f then
    if Filename.check_suffix f ".i" ||
    Filename.check_suffix f ".c" then
      files := f :: !files
    else
      let _ = prerr_endline ("Error: " ^ f ^ ": Not a preprocessed C") in
      exit 1
  else
    let _ = prerr_endline ("Error: " ^ f ^ ": No such file") in
    exit 1

let parseOneFile : string -> C.file
= fun fname ->
  (* PARSE and convert to CIL *)
  if !Cilutil.printStages then ignore (E.log "Parsing %s\n" fname);
  let cil = F.parse fname () in
  if not (Feature.enabled "epicenter") then (
    (Rmtmps.removeUnusedTemps cil)
  );
  cil

let parse : unit -> C.file
= fun () ->
  match List.map parseOneFile !files with
    [one] -> one
  | [] -> (prerr_endline "Error: No arguments are given"; exit 1)
  | files ->
    Mergecil.ignore_merge_conflicts := true;
    let merged = Stats.time "merge" (Mergecil.merge files) "merged" in
      if !E.hadErrors then
        E.s (E.error "There were errors during merging");
      merged

let makeCFGinfo : Cil.file -> Cil.file
=fun f ->
  ignore (Partial.calls_end_basic_blocks f) ;
  ignore (Partial.globally_unique_vids f) ;
  Cil.iterGlobals f (fun glob -> match glob with
    Cil.GFun(fd,_) ->
                  Cil.prepareCFG fd ;
                  (* jc: blockinggraph depends on this "true" arg *)
                  ignore (Cil.computeCFGInfo fd true)
  | _ -> ());
  f

(* true if the given function has variable number of arguments *)
let is_varargs : string -> Cil.file -> bool
=fun fid file ->
  Cil.foldGlobals file (fun b global ->
    match global with
    | GFun (fd,_) when fd.svar.vname = fid ->
        (match fd.svar.vtype with
        | TFun (_,_,b_va,_) -> b_va
        | _ -> b)
    | _ -> b
  ) false

let inline : Global.t -> bool
=fun global ->
  let f = global.file in
  let regexps = List.map (fun str -> Str.regexp (".*" ^ str ^ ".*")) !Options.inline in
  let to_inline =
    list_fold (fun global to_inline ->
      match global with
      | GFun (fd,_) when List.exists (fun regexp -> Str.string_match regexp fd.svar.vname 0) regexps ->
        fd.svar.vname :: to_inline
      | _ -> to_inline
    ) f.globals [] in
  let varargs_procs = List.filter (fun fid -> is_varargs fid f) to_inline in
  let recursive_procs = List.filter (fun fid -> Global.is_rec fid global) to_inline in
  let large_procs = List.filter (fun fid -> try List.length (InterCfg.nodes_of_pid global.icfg fid) > !Options.inline_size with _ -> false) to_inline in
  let to_exclude = varargs_procs @ recursive_procs @ large_procs in
  prerr_endline ("To inline : " ^ Vocab.string_of_list Vocab.id to_inline);
  prerr_endline ("Excluded variable-arguments functions : " ^ Vocab.string_of_list Vocab.id varargs_procs);
  prerr_endline ("Excluded recursive functions : " ^ Vocab.string_of_list Vocab.id recursive_procs);
  prerr_endline ("Excluded too large functions : " ^ Vocab.string_of_list Vocab.id large_procs);
  Inline.toinline := List.filter (fun fid -> not (List.mem fid to_exclude)) to_inline;
  Inline.doit f;
  not (!Inline.toinline = [])
