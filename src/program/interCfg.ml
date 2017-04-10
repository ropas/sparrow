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
(** Inter-procedural CFG *)

open Vocab
open Cil
open Yojson.Safe
open IntraCfg.Cmd

module Proc =
struct
  include String
  let equal = (=)
  let hash = Hashtbl.hash
  let to_string x = x
end

module ProcSet = BatSet.Make(Proc)
type pid = Proc.t
module Node = struct
  type t = Proc.t * IntraCfg.Node.t [@@deriving compare]
  let to_string (pid,node) = pid ^ "-" ^ IntraCfg.Node.to_string node
  let to_json x = `String (to_string x)
  let make pid node = (pid,node)
  let get_pid (pid,node) = pid
  let get_cfgnode (pid,node) = node
  let hash = Hashtbl.hash
  let equal (p1, n1) (p2, n2) = p1 = p2 && IntraCfg.Node.equal n1 n2
end

module IntraNodeSet = IntraCfg.NodeSet
module NodeSet = BatSet.Make(Node)

type node = Node.t

type t = {
  cfgs        : (pid, IntraCfg.t) BatMap.t;
  globals     : Cil.global list;
  call_edges  : (Node.t, ProcSet.t) BatMap.t;
}

let dummy = {
  cfgs = BatMap.empty;
  globals = [];
  call_edges = BatMap.empty;
}

let add_call_edge : Node.t -> Proc.t -> t -> t
= fun call_node pid g -> 
  let callees = (try BatMap.find call_node g.call_edges with _ -> ProcSet.empty)
    |> ProcSet.add pid  
  in
  { g with call_edges = BatMap.add call_node callees g.call_edges }

let get_callees : Node.t -> t -> ProcSet.t
= fun call_node g ->
  try BatMap.find call_node g.call_edges with _ -> ProcSet.empty 

let global_proc = "_G_"
let start_node = Node.make global_proc IntraCfg.Node.entry

let gen_cfgs file = 
  BatMap.add global_proc (IntraCfg.generate_global_proc file.Cil.globals (Cil.emptyFunction global_proc))
    (list_fold (fun g m ->
      match g with
      | Cil.GFun (f,loc) -> BatMap.add f.svar.vname (IntraCfg.init f loc) m
      | _ -> m
    ) file.Cil.globals BatMap.empty)

let compute_dom_and_scc icfg = 
  { icfg with cfgs = 
      BatMap.map (fun cfg ->
        IntraCfg.compute_scc (IntraCfg.compute_dom cfg)
      ) icfg.cfgs }

let remove_function : pid -> t -> t
=fun pid icfg -> 
  let is_not_pid_node node _ = Node.get_pid node <> pid in
  { icfg with cfgs = BatMap.remove pid icfg.cfgs ;
    call_edges = BatMap.filter is_not_pid_node icfg.call_edges }

let cfgof : t -> pid -> IntraCfg.t 
=fun g pid -> 
  try BatMap.find pid g.cfgs with Not_found -> prerr_endline ("InterCfg.cfgof "^pid); raise Not_found

let cmdof : t -> Node.t -> IntraCfg.cmd
=fun g (pid,node) -> IntraCfg.find_cmd node (cfgof g pid)

let add_cmd : t -> Node.t -> IntraCfg.cmd -> t
=fun g (pid,node) cmd -> 
  {g with cfgs = BatMap.add pid (IntraCfg.add_cmd node cmd (cfgof g pid)) g.cfgs}

let nodes_of_pid : t -> pid -> Node.t list
=fun g pid -> List.map (Node.make pid) (IntraCfg.nodesof (cfgof g pid))

let fold_cfgs f g a = BatMap.foldi f g.cfgs a
let map_cfgs f g = {g with cfgs = BatMap.map f g.cfgs}

let nodesof : t -> Node.t list
=fun g ->
  BatMap.foldi (fun pid cfg ->
    List.append 
        (List.map (fun n -> Node.make pid n) (IntraCfg.nodesof cfg))
  ) g.cfgs []

let pidsof : t -> pid list
=fun g ->
  BatMap.foldi (fun pid _ acc -> pid :: acc) g.cfgs []

let is_undef : pid -> t -> bool
=fun pid g ->
  not (BatMap.mem pid g.cfgs)

let is_entry = function (_, node) -> IntraCfg.is_entry node
let is_exit = function (_, node) -> IntraCfg.is_exit node

let is_callnode : node -> t -> bool
=fun (pid,node) g -> IntraCfg.is_callnode node (cfgof g pid)

let is_returnnode : node -> t -> bool
=fun (pid,node) g -> 
  try IntraCfg.is_returnnode node (cfgof g pid) with _ -> false

let returnof : node -> t -> node 
=fun (pid,node) g -> (pid, IntraCfg.returnof node (cfgof g pid))

let is_inside_loop : node -> t -> bool
=fun (pid,node) g -> IntraCfg.is_inside_loop node (cfgof g pid)

let callof : node -> t -> node
=fun (pid,node) g -> (pid, IntraCfg.callof node (cfgof g pid))

let argsof : t -> pid -> Cil.varinfo list
=fun g pid -> IntraCfg.get_formals (cfgof g pid)

let callnodesof : t -> node list
=fun g -> List.filter (fun node -> is_callnode node g) (nodesof g)

let entryof : t -> pid -> node
=fun g pid -> Node.make pid IntraCfg.Node.entry

let exitof : t -> pid -> node
=fun g pid -> Node.make pid IntraCfg.Node.exit

let unreachable_node_pid : pid -> IntraCfg.t -> NodeSet.t
=fun pid icfg ->
  IntraNodeSet.fold (fun node -> NodeSet.add (pid, node)) 
    (IntraCfg.unreachable_node icfg) NodeSet.empty

let unreachable_node : t -> NodeSet.t
=fun g ->
  let add_unreachable_node pid icfg =
    NodeSet.union (unreachable_node_pid pid icfg) in
  fold_cfgs add_unreachable_node g NodeSet.empty

let remove_node : node -> t -> t
=fun (pid, intra_node) g ->
  let intra_cfg = cfgof g pid in
  let intra_cfg = IntraCfg.remove_node intra_node intra_cfg in
  { g with cfgs = BatMap.add pid intra_cfg g.cfgs }

let print : out_channel -> t -> unit
=fun chan g -> BatMap.iter (fun pid cfg -> IntraCfg.print_dot chan cfg) g.cfgs

let to_json : t -> json
= fun g ->
  `Assoc (
    BatMap.foldi (fun pid cfg json ->
      (pid, IntraCfg.to_json cfg)::json) g.cfgs [])

let print_json : out_channel -> t -> unit
= fun chan g ->
  Yojson.Safe.pretty_to_channel chan (to_json g)

(*
  1. collect all strings in salloc(_,s) of program: S
  2. build a map from s \in S to lv: M
  3. replace each salloc(lv,s) by set(lv,M(s))
  4. insert salloc(lv,s) in _G_ for each (s,lv) \in M
*)

let collect_strs : t -> string BatSet.t 
=fun icfg ->
  list_fold (fun n -> 
    match cmdof icfg n with
    | Csalloc (_,s,_) -> BatSet.add s
    | _ -> id
  ) (nodesof icfg) BatSet.empty

let str_count = ref 0
let get_cstr_name () =
  str_count := !str_count + 1;
  "_zoo_cstr_" ^ i2s !str_count

let build_strmap : string BatSet.t -> (string, lval) BatMap.t
=fun strs ->
  BatSet.fold (fun str map ->
    let name = get_cstr_name () in
    let lv = (Var (Cil.makeGlobalVar name Cil.charPtrType), NoOffset) in
      BatMap.add str lv map
  ) strs BatMap.empty

let replace_salloc : t -> (string, lval) BatMap.t -> t
=fun icfg strmap ->
  let nodes = nodesof icfg in
    list_fold (fun n g -> 
      match cmdof g n with
      | Csalloc (lhs,str,location) -> 
        let rhs = Lval (BatMap.find str strmap) in
        let cmd = Cset (lhs,rhs,location) in
          add_cmd g n cmd
      | _ -> g
    ) nodes icfg

let dummy_location = { line = 0; file = ""; byte = 0 }

let insert_salloc : t -> (string, lval) BatMap.t -> t 
=fun icfg strmap ->
  let _G_ = cfgof icfg global_proc in
  let _G_with_sallocs = 
    BatMap.foldi (fun str lhs g ->
      let entry = IntraCfg.entryof g in
      let _ = assert (List.length (IntraCfg.succ entry g) = 1) in
      let next = List.nth (IntraCfg.succ entry g) 0 in
      let cmd = Csalloc (lhs,str,dummy_location) in
        IntraCfg.add_new_node entry cmd next g
    ) strmap _G_ in
  { icfg with cfgs = BatMap.add global_proc _G_with_sallocs icfg.cfgs}

let opt_salloc : t -> t
=fun icfg -> 
  let strs = collect_strs icfg in
  let strmap = build_strmap strs in
  let icfg = replace_salloc icfg strmap in
  let icfg = insert_salloc icfg strmap in
    icfg

let optimize_il : t -> t
=fun icfg ->
  icfg 
  |> map_cfgs IntraCfg.optimize
(*  |> opt_salloc*)

let init : Cil.file -> t
=fun file -> 
  { cfgs = gen_cfgs file; globals = file.Cil.globals ; call_edges = BatMap.empty } 
  |> opt !Options.opt_optil optimize_il
  |> compute_dom_and_scc

