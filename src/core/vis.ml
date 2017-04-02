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
open Printf

module Json = Yojson.Safe

let opts = []
let usage = ""

let file = ref ""

let args f = 
  if Sys.file_exists f then 
    file := f
  else 
    raise (Arg.Bad (f^": No such file"))

let dump_nodes : out_channel -> (string * Json.json) list -> unit
= fun chan l ->
  List.iter (fun (node, attr) ->
      match attr with 
        `List [cmd; `Bool loophead; `Bool callnode] ->
          let cmd = Json.to_string cmd in
          let cmd = if String.length cmd > 2 then String.sub cmd 1 ((String.length cmd) -2) else "" in
          let str = (node^"[label=\""^node^": "^cmd^"\""^
             (if loophead then " style=filled color=lightblue" else "")^ 
             (if callnode then " style=filled color=grey" else "")^"]\n")
          in
          output_string chan str
      | _ -> raise (Failure "error")) l;
  fprintf chan "}\n"

let dump_edges : out_channel -> Json.json list -> unit
= fun chan l -> 
  List.iter (fun edge ->
      match edge with 
        `List [`String v1; `String v2] ->
          fprintf chan "%s -> %s\n" v1 v2
      | _ -> raise (Failure "error")
    ) l


let dump_dug : out_channel -> string -> (string * Json.json) list 
  -> (string * string, string) BatMap.t -> unit
= fun chan pid l dug ->
  List.iter (fun (src, _) ->
    List.iter (fun (dst, _) ->
      try
        let (psrc, pdst) = (pid^"-"^src, pid^"-"^dst) in
        let label = BatMap.find (psrc, pdst) dug in
        fprintf chan "%s -> %s [label=\"%s\" color=red]\n" src dst label
(*        fprintf chan "%s -> %s [tooltip=\"%s\" color=red]\n" src dst label*)
      with _ -> ()) l) l

let dump_cfgs : Json.json -> unit
= fun json ->
  match json with 
    `Assoc l -> 
      List.iter (fun (pid, cfg) ->
        let dot = pid^".dot" in
        let chan = open_out dot in
        fprintf chan "digraph %s {\n" pid;
        fprintf chan "{\n";
        fprintf chan "node [shape=box]\n";
        (match cfg with 
         `Assoc [(_, `Assoc nodes); (_, `List edges)] ->
         dump_nodes chan nodes;
         dump_edges chan edges
         | _ -> raise (Failure "error"));
        fprintf chan "}\n";
        close_out chan;
        let _ = Unix.create_process "dot" [|"dot"; "-Tsvg"; "-o"^pid^".svg"; dot|] Unix.stdin Unix.stdout Unix.stderr in
        let _ = Unix.wait () in
          ()
        ) l
  | _ -> raise (Failure "Invalid json format")

let dump_cfgs_with_dug : Json.json -> (string * string, string) BatMap.t -> unit
= fun json dug ->
  match json with 
    `Assoc l -> 
      List.iter (fun (pid, cfg) ->
        let dot = pid^".dot" in
        let chan = open_out dot in
        fprintf chan "digraph %s {\n" pid;
        fprintf chan "{\n";
        fprintf chan "node [shape=box]\n";
        (match cfg with 
         `Assoc [(_, `Assoc nodes); (_, `List edges)] ->
         dump_nodes chan nodes;
         dump_edges chan edges;
         dump_dug chan pid nodes dug;
         | _ -> raise (Failure "error"));
        fprintf chan "}\n";
        close_out chan;
        let _ = Unix.create_process "dot" [|"dot"; "-Tsvg"; "-o"^pid^".svg"; dot|] Unix.stdin Unix.stdout Unix.stderr in
        let _ = Unix.wait () in
          ()
        ) l
  | _ -> raise (Failure "Invalid json format")


let dump_callgraph : Json.json -> unit
= fun json ->
  let index = "callgraph.dot" in
  let chan = open_out index in
  fprintf chan "digraph %s {\n" "callgraph";
  fprintf chan "{\n";
  fprintf chan "node [shape=box]\n";
  (match json with 
    `Assoc l -> 
      (match (List.assoc "nodes" l, List.assoc "edges" l) with
        (`List nodes, `List edges) -> 
          List.iter (fun node ->
            match node with 
              `String s -> fprintf chan ("%s[label=\"%s\" URL=\"%s\"]\n") s s (s^".svg")
            | _ -> raise (Failure "error")) nodes;
          fprintf chan "}\n";
          List.iter (fun edge ->
            match edge with 
              `List [`String v1; `String v2] -> fprintf chan "%s -> %s\n" v1 v2
            | _ -> raise (Failure "error")) edges;
          fprintf chan "}\n"
       | _ -> raise (Failure "Invalid json format"))
  | _ -> raise (Failure "Invalid json format"));
  close_out chan;
  let _ = Unix.create_process "dot" [|"dot"; "-Tsvg"; "-ocallgraph.svg"; index|] Unix.stdin Unix.stdout Unix.stderr in
  let _ = Unix.wait () in
    ()

let create_index : unit -> unit
= fun () ->
  let index = "index.html" in
  let chan = open_out index in
  fprintf chan "<html>\n";
  fprintf chan "<head>\n";
  fprintf chan "<meta http-equiv=\"refresh\" content=\"0; url=callgraph.svg\" />";
  fprintf chan "</head>\n";
  fprintf chan "<body>\n";
  fprintf chan "</body>\n";
  fprintf chan "</html>\n";
  close_out chan

let gen_dug : Json.json -> (string * string, string) BatMap.t
= fun json ->
  match json with 
    `Assoc dug -> 
      let edges = List.assoc "edges" dug in
      (match edges with 
      `List l ->
      List.fold_left (fun m e -> 
          match e with 
            `List [`String src; `String dst; `String label] ->
              BatMap.add (src,dst) label m
          | _ -> m) BatMap.empty l
      | _ -> raise (Failure "error"))
   | _ -> raise (Failure "error")
  
let dump : Json.json -> unit 
= fun json ->
  let dir = !file^".vis" in
  (try Unix.mkdir dir 0o755 with _ -> ());
  Unix.chdir dir;
  create_index ();
  match json with 
    `Assoc global ->
      let dug = try gen_dug (List.assoc "dugraph" global) with _ -> BatMap.empty in
      dump_callgraph (List.assoc "callgraph" global);
      dump_cfgs_with_dug (List.assoc "cfgs" global) dug;
  | _ -> raise (Failure "error")



let main () = 
  Arg.parse opts args usage;
  let json = Json.from_file !file in
(*  Json.pretty_to_channel stdout json;*)
  dump json

let _ = main ()
