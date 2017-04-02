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
open AbsSem
open BasicDom
open ItvDom

module AccessSem = 
struct
  include AccessSem.Make(ItvSem)
  let accessof_eval pid e mem = 
    Dom.init_access ();
    let _ = ItvSem.eval pid e mem in
    Dom.return_access ()
end
module AccessAnalysis = AccessAnalysis.Make(AccessSem)
module Analysis = SparseAnalysis.Make(ItvSem)
module Access = ItvSem.Dom.Access

type loop = string

type feature = {
  (* syntactic features *)
  null : bool;                   (* Whether the loop condition contains nulls or not *)
  constant : bool;               (* Whether the loop condition contains constants or not *)
  array_content : bool;          (* Whether the loop condition contains array accesses or not *)
  conjunction : bool;            (* Whether the loop condition contains && or not *)
  single_index : bool;           (* Whether the loop condition contains an index for a single array in the loop *)
  multi_index : bool;            (* Whether the loop condition contains an index for multiple arrays in the loop *)
  out_index : bool;              (* Whether the loop condition contains an index for an array outside of the loop *)
  init_index : bool;             (* Whether an index is initialized before the loop e.g.) i = 0, p = arr, etc *)
  exits : float;                 (* The normalized number of exits in the loop *)
  scc_size : float;              (* The normalized size of the loop (# nodes) *)
  diff_array_access : float;     (* The normalized number of distinct array accesses in the loop *)
  idx_pp : float;                (* The normalized number of arithmetic increments in the loop *)
  ptr_pp : float;                (* The normalized number of pointer increments in the loop *)
  (* semantic features *)
  prune_cond : bool;             (* Whether the loop condition prunes the abstract state or not *)
  extern : bool;                 (* Whether the loop condition is determined by external inputs *)
  gvar : bool;                   (* Whether global variables are accessed in the loop condition *)
  finite_itv : bool;             (* Whether a variable has a finite interval value in the loop condition *)
  finite_arr : bool;             (* Whether a variable has a finite size of array in the loop condition *)
  cstring : bool;                (* Whether a variable has a finite string in the loop condition *)
  close_left_arr_size : bool;    (* Whether a variable has an array of which the size is a left-closed interval *)
  close_left_arr_offset : bool;  (* Whether a variable has an array of which the offset is a left-closed interval *)
  points_to : float;             (* The (normalized) number of abstract locations accessed in the loop *)
}

type data = (loop, feature) BatMap.t

let empty_feature = {
  null = false;
  constant = false;
  array_content = false;
  conjunction = false;
  single_index = false;
  multi_index = false;
  out_index = false;
  init_index = false;
  exits = 0.0;
  scc_size = 0.0;
  diff_array_access = 0.0;
  idx_pp =0.0;
  ptr_pp =0.0;

  prune_cond = false;
  extern = false;
  gvar = false;
  finite_itv = false;
  finite_arr = false;
  close_left_arr_size = false;
  close_left_arr_offset = false;
  points_to = 0.0;
  cstring = false;
}

let string_of_feature f =
  "null : "^(string_of_bool f.null)^"\n"^
  "constant : "^(string_of_bool f.constant)^"\n"^
  "array_content : "^(string_of_bool f.array_content)^"\n"^
  "conjunction : "^(string_of_bool f.conjunction)^"\n"^
  "single index : "^(string_of_bool f.single_index)^"\n"^
  "multi index : "^(string_of_bool f.multi_index)^"\n"^
  "out index : "^(string_of_bool f.out_index)^"\n"^
  "init index : "^(string_of_bool f.init_index)^"\n"^
  "exits : "^(string_of_float f.exits)^"\n"^
  "scc_size : "^(string_of_float f.scc_size)^"\n"^
  "array access : "^(string_of_float f.diff_array_access)^"\n"^
  "idx_pp : "^(string_of_float f.idx_pp)^"\n"^
  "ptr_pp : "^(string_of_float f.ptr_pp)^"\n"^
  "prune cond : "^(string_of_bool f.prune_cond)^"\n"^
  "extern : "^(string_of_bool f.extern)^"\n"^
  "gvar : "^(string_of_bool f.gvar)^"\n"^
  "finite_itv : "^(string_of_bool f.finite_itv)^"\n"^
  "finite_arr : "^(string_of_bool f.finite_arr)^"\n"^
  "close_left_arr_size : "^(string_of_bool f.close_left_arr_size)^"\n"^
  "close_left_arr_offset : "^(string_of_bool f.close_left_arr_offset)^"\n"^
  "points_to : "^(string_of_float f.points_to)^"\n"^
  "cstring : "^(string_of_bool f.cstring)^"\n"^
  "\n"

let b2is b = if b then "1" else "0"
let string_of_raw_feature f = 
  (b2is f.null)^"\t"^
  (b2is f.constant)^"\t"^
  (b2is f.array_content)^"\t"^
  (b2is f.conjunction)^"\t"^
  (b2is f.single_index)^"\t"^
  (b2is f.multi_index)^"\t"^
  (b2is f.out_index)^"\t"^
  (b2is f.init_index)^"\t"^
  (string_of_float f.exits)^"\t"^
  (string_of_float f.scc_size)^"\t"^
  (string_of_float f.diff_array_access)^"\t"^
  (string_of_float f.idx_pp)^"\t"^
  (string_of_float f.ptr_pp)^"\t"^
  (b2is f.prune_cond)^"\t"^
  (b2is f.extern)^"\t"^
  (b2is f.gvar)^"\t"^
  (b2is f.finite_itv)^"\t"^
  (b2is f.finite_arr)^"\t"^
  (b2is f.close_left_arr_size)^"\t"^
  (b2is f.close_left_arr_offset)^"\t"^
  (string_of_float f.points_to)^"\t"^
  (b2is f.cstring)^"\t"^
  "\t"

let float_of_bool b = if b then 1.0 else 0.0
let feature_vector_of f = 
  [(float_of_bool f.null);
   (float_of_bool f.constant);
   (float_of_bool f.array_content);
   (float_of_bool f.conjunction);
   (float_of_bool f.single_index);
   (float_of_bool f.multi_index);
   (float_of_bool f.out_index);
   (float_of_bool f.init_index);
   f.exits;
   f.scc_size;
   f.diff_array_access;
   f.idx_pp;
   f.ptr_pp;
   (float_of_bool f.prune_cond);
   (float_of_bool f.extern);
   (float_of_bool f.gvar);
   (float_of_bool f.finite_itv);
   (float_of_bool f.finite_arr);
   (float_of_bool f.close_left_arr_size);
   (float_of_bool f.close_left_arr_offset);
   (f.points_to);
   (float_of_bool f.cstring)]
 
let string_of_trset trset = 
  BatMap.foldi (fun k v s -> 
      s^k^"\t"^(string_of_raw_feature v)^"\n") trset ""

let sem_fun = ItvSem.run AbsSem.Strong ItvSem.Spec.empty 

let add_prune_cond global cond feat = 
  let (output,_) = sem_fun cond (global.mem, global) in
  let pruned = Mem.exists (fun k v ->
        let v2 = Mem.find k output in
        let itv1 = Val.itv_of_val v in
        let itv2 = Val.itv_of_val v2 in
        Itv.le itv2 itv1 && not (Itv.eq itv1 itv2)
      ) global.mem
  in
  if pruned then { feat with prune_cond = true }
  else feat

let add_extern global cond feat =
  let locset = AccessSem.accessof global cond sem_fun global.mem |> Access.accessof in
  let locset = Val.pow_loc_of_val (Mem.lookup locset global.mem) in 
  if PowLoc.exists Loc.is_ext_allocsite locset then { feat with extern = true }
  else feat

let incptr_itself_by_one (lv,e) = 
  match lv,e with
  | (Var x, NoOffset), (BinOp (PlusPI, Lval (Var y,NoOffset),Const (CInt64 (i,_,_)),_)) 
     when x.vname = y.vname && Cil.i64_to_int i = 1 -> true
  | _ -> false

let add_cstring global cond_node cfg feat =
  let pid = Node.get_pid cond_node in 
  let access = AccessSem.accessof global cond_node sem_fun global.mem in
  let locset = Access.accessof access in
  (* while (c = *x++)   ==>  tmp = x; x++; c = *tmp; while(c) *)
  let locset = 
    let pred0 = try IntraCfg.pred (Node.get_cfgnode cond_node) cfg with _ -> [] in
    let pred1 = try IntraCfg.pred (List.hd pred0) cfg with _ -> [] in
    let pred2 = try IntraCfg.pred (List.hd pred1) cfg with _ -> [] in
    let pred3 = try IntraCfg.pred (List.hd pred2) cfg with _ -> [] in
    if (List.length pred1 = 1) && (List.length pred2 = 1) && (List.length pred3 = 1) then
      let cmd1 = IntraCfg.find_cmd (List.hd pred1) cfg in
      let cmd2 = IntraCfg.find_cmd (List.hd pred2) cfg in
      let cmd3 = IntraCfg.find_cmd (List.hd pred3) cfg in
      match (cmd1, cmd2, cmd3) with 
        (IntraCfg.Cmd.Cset ((Var c, NoOffset), e1, _),
         IntraCfg.Cmd.Cset (lv2, e2,_),
         IntraCfg.Cmd.Cset (lv3, Lval lv4, _)) 
          when  incptr_itself_by_one (lv2, e2) && lv2 = lv4 -> 
          PowLoc.join locset (ItvSem.eval_lv pid lv2 global.mem )
      | _ -> locset
    else locset
  in
  PowLoc.fold (fun l feat ->
      let v = Mem.lookup (PowLoc.singleton l) global.mem in
      let nullpos = ArrayBlk.nullof (Val.array_of_val v) in 
      let locset = Val.pow_loc_of_val (Mem.lookup (PowLoc.singleton l) global.mem) in 
      let locset = PowLoc.fold (fun x -> BatSet.add (Loc.to_string x)) locset BatSet.empty in
      let strlib = List.map (fun x -> "__extern__"^x) ["getenv"; "rindex"; "index"; "strdup"; "strrchr"; "strstr"; "basename" ; "strtok" ; "fgets"; "_IO_getc"] in
      if (BatSet.exists (fun x -> List.mem x strlib) locset) || 
         (not (Itv.is_bot nullpos) && (Itv.is_finite nullpos))
      then 
        { feat with cstring = true }
      else feat) locset feat

let add_gvar global cond feat =
  let access = AccessSem.accessof global cond sem_fun global.mem in
  let locset = Access.accessof access in
  let locset = PowLoc.join locset (Val.pow_loc_of_val (Mem.lookup locset global.mem)) in 
  let locset = PowLoc.remove Loc.null locset in
  if (PowLoc.exists Loc.is_gvar locset) || 
     (PowLoc.exists (fun x -> (String.sub (Loc.to_string x) 0 3) = "_G_") locset)
  then 
    { feat with gvar = true }
  else feat

let get_cond_exp cond cfg = 
  let cmd = IntraCfg.find_cmd (InterCfg.Node.get_cfgnode cond) cfg in
  match cmd with 
    IntraCfg.Cmd.Cassume (e, _) -> e
  | _ -> raise (Failure ("get_cond_exp : "^(IntraCfg.Cmd.to_string cmd)))

let add_array_content global cond cfg feat = 
  let exists_deref_lval = function
      (Var _, offset) as lval ->
      let (_, offset) = Cil.removeOffsetLval lval in
      begin match offset with Index (_, _) -> true | _ -> false end
    | (Mem _, NoOffset) -> true
    | (_, _) -> false
  in
  let rec exists_deref = function 
      Lval lval -> exists_deref_lval lval
    | UnOp (_, e, _) -> exists_deref e
    | BinOp (_, e1, e2, _) -> (exists_deref e1) || (exists_deref e2)
    | Question (e, _, _, _) | CastE (_, e) -> exists_deref e
    | _ -> false
  in
  if exists_deref cond then {feat with array_content = true }
  else feat
 
let add_null cond cfg feat = 
  let rec null_condition = function
    | UnOp (LNot, e, _) -> null_condition e
    | Lval _ -> true
    | BinOp (bop, e1, e2, _) 
      when bop = Eq || bop = Ne -> (Cil.isZero e1) || (Cil.isZero e2)
    | _ -> false
  in
  if null_condition (CilHelper.remove_cast cond) then
    { feat with null = true }
  else feat
 
let add_constant cond cfg feat = 
  let rec const_condition = function
    | UnOp (_, e, _) -> const_condition e
    | BinOp (bop, Lval _, (Const _ as c), _) 
    | BinOp (bop, (Const _ as c), Lval _, _) 
      when bop = Lt || bop = Gt || bop = Le || bop = Ge 
      || bop = Eq || bop = Ne -> Cil.zero <> c
    | _ -> false
  in
  if const_condition (CilHelper.remove_cast cond) then
    { feat with constant = true }
  else feat
 
let add_conjunction cond_node cfg feat = 
  let pred = IntraCfg.pred (Node.get_cfgnode cond_node) cfg in
  if List.length pred = 1 then 
    let pred = IntraCfg.pred (List.hd pred) cfg in
    match IntraCfg.find_cmd (List.hd pred) cfg with 
      IntraCfg.Cmd.Cassume _ -> 
        { feat with conjunction = true }
    | _ -> feat
  else feat

let add_finite_itv global cond_node feat = 
  let access = PowLoc.remove Loc.null (Access.accessof (AccessSem.accessof global cond_node sem_fun global.mem)) in
  PowLoc.fold (fun loc feat -> 
      let v = Mem.lookup (PowLoc.singleton loc) global.mem in
      let itv = Val.itv_of_val v in
      let ploc = PowLoc.remove Loc.null (Val.pow_loc_of_val v) in
      if (Itv.is_finite itv) && (PowLoc.bot = ploc) then 
        { feat with finite_itv = true }
      else feat) access feat

let add_close_left_arr_size_offset global cond_node cfg feat = 
  let pid = Node.get_pid cond_node in 
  let node = Node.get_cfgnode cond_node in 
  let cmd = IntraCfg.find_cmd node cfg in
  let qs = AlarmExp.collect cmd in
  List.fold_left (fun feat q ->
        match q with 
          AlarmExp.ArrayExp (arr, i, _)
        | AlarmExp.DerefExp (BinOp (_, Lval arr, i, _), _) ->
            let v1 = Mem.lookup (ItvSem.eval_lv pid arr global.mem) global.mem in
            let size = ArrayBlk.sizeof (Val.array_of_val v1) in
            let offset = ArrayBlk.offsetof (Val.array_of_val v1) in
            let feat = 
              if (Itv.open_right size) && (Itv.close_left size) then 
                { feat with close_left_arr_size = true } 
              else if Itv.is_finite size then 
                { feat with finite_arr = true } 
              else feat 
            in
            if (Itv.open_right offset) && (Itv.close_left offset) then 
            begin
              { feat with close_left_arr_offset = true } 
            end
            else feat
        | _ -> feat) feat qs

let add_points_to global conds feat =
  let locset = List.fold_left (fun locset cond ->
    let access = AccessSem.accessof global cond sem_fun global.mem in
    PowLoc.join (Access.accessof access) locset) PowLoc.bot conds
  in
  { feat with points_to = float_of_int (PowLoc.cardinal locset) }

let add_exits conds feat = { feat with exits = float_of_int (List.length conds) }
let add_scc_size scc feat = { feat with scc_size = float_of_int (List.length scc) }

let add_index conds cfg scc feat = 
  let rec collect_lv = function 
    | (Lval (Var _, NoOffset)) as x -> BatSet.singleton x
    | Lval (Mem e, _) -> collect_lv e
    | SizeOfE e -> collect_lv e
    | UnOp (_, e, _) -> collect_lv e
    | BinOp (_, e1, e2, _) -> BatSet.union (collect_lv e1) (collect_lv e2)
    | Question (e1, e2, e3, _) -> BatSet.union (BatSet.union (collect_lv e1) (collect_lv e2)) (collect_lv e3)
    | _ -> BatSet.empty 
  in
  let lvset = List.fold_left (fun lvset node -> 
      let cond_exp = CilHelper.remove_cast (get_cond_exp node cfg) in
      BatSet.union lvset (collect_lv cond_exp)) BatSet.empty conds in
  let qs = List.fold_left (fun qs node ->
      let cmd = IntraCfg.find_cmd node cfg in
      qs@(AlarmExp.collect cmd)) [] scc in
  let map = List.fold_left (fun map q ->
        match q with 
          AlarmExp.ArrayExp (arr, i, _)
        | AlarmExp.DerefExp (BinOp (_, Lval arr, i, _), _) ->
            let s = try BatMap.find i map with _ -> BatSet.empty in
            let s = BatSet.add arr s in
            BatMap.add i s map
        | _ -> map) BatMap.empty qs in
  if BatMap.is_empty map then feat
  else if BatMap.for_all (fun i set -> 
            (BatSet.mem i lvset) && (BatSet.cardinal set = 1)) map then
    { feat with single_index = true }
  else if BatMap.for_all (fun i set -> BatSet.mem i lvset) map then
    { feat with multi_index = true }
  else feat

let add_out_index conds cfg scc feat = 
  let qs = List.fold_left (fun qs node ->
      let cmd = IntraCfg.find_cmd node cfg in
      qs@(AlarmExp.collect cmd)) [] scc in
  let idx_in_scc = List.fold_left (fun set q ->
        match q with 
          AlarmExp.ArrayExp (arr, i, _)
        | AlarmExp.DerefExp (BinOp (_, Lval arr, i, _), _) ->
            BatSet.add i set
        | _ -> set) BatSet.empty qs in
  let idx_in_scc = List.fold_left (fun set node ->
      let cmd = IntraCfg.find_cmd node cfg in 
      match cmd with 
        IntraCfg.Cmd.Cset (tmp, ((Lval (Var _, NoOffset)) as i), _) ->
          if BatSet.mem (Lval tmp) idx_in_scc then 
            BatSet.add i set
          else set
      | _ -> set) idx_in_scc scc in
  let exits = List.fold_left (fun exits node ->
                let succ = IntraCfg.succ node cfg in
                let outs = List.filter (fun x -> not (List.mem x scc)) succ in
                exits@outs) [] scc 
  in
  let exits_nexts = 
    List.fold_left (fun l n -> 
            l@(IntraCfg.succ n cfg)) [] exits
  in
  let qs = List.fold_left (fun qs node ->
      let cmd = IntraCfg.find_cmd node cfg in
      qs@(AlarmExp.collect cmd)) [] exits_nexts in
  let idx_out_loop = List.fold_left (fun set q ->
        match q with 
          AlarmExp.ArrayExp (arr, i, _)
        | AlarmExp.DerefExp (BinOp (_, Lval arr, i, _), _) ->
            BatSet.add i set
        | _ -> set) BatSet.empty qs in
  if not (BatSet.is_empty (BatSet.intersect idx_out_loop idx_in_scc)) then
    { feat with out_index = true }
  else feat

let add_init global conds cfg scc feat = 
  let pid = Node.get_pid (List.hd conds) in 
  let entries = List.fold_left (fun entries node ->
                let preds = IntraCfg.pred node cfg in
                let ins = List.filter (fun x -> not (List.mem x scc)) preds in
                entries@ins) [] scc 
  in
  let entry_prev = 
    List.fold_left (fun l n -> l@(IntraCfg.pred n cfg)) entries entries in
  let entry_prev = 
    List.fold_left (fun l n -> l@(IntraCfg.pred n cfg)) entry_prev entry_prev in
   let qs = List.fold_left (fun qs node ->
      let cmd = IntraCfg.find_cmd node cfg in
      qs@(AlarmExp.collect cmd)) [] scc in
  let idx_in_scc = List.fold_left (fun set q ->
      match q with 
        AlarmExp.ArrayExp (_, i, _)
      | AlarmExp.DerefExp (BinOp (_, _, i, _), _) ->
          PowLoc.join set (Access.useof (AccessSem.accessof_eval pid i global.mem)) 
      | _ -> set) PowLoc.bot qs in
  let buf_in_scc = List.fold_left (fun set q ->
      match q with 
        AlarmExp.ArrayExp (arr, _, _) -> 
          PowLoc.join set (Access.useof (AccessSem.accessof_eval pid (Lval arr) global.mem)) 
      | AlarmExp.DerefExp (BinOp (_, arr, _, _), _)
      | AlarmExp.DerefExp (arr, _) -> 
          PowLoc.join set (Access.useof (AccessSem.accessof_eval pid arr global.mem)) 
      | _ -> set) PowLoc.bot qs in
  let feat = List.fold_left (fun feat node ->
      let cmd = IntraCfg.find_cmd node cfg in
      match cmd with 
        IntraCfg.Cmd.Cset (x, _, _) ->
          let set = PowLoc.remove Loc.null (ItvSem.eval_lv pid x global.mem) in
          if PowLoc.bot <> (PowLoc.meet set idx_in_scc) then 
            { feat with init_index = true }
          else if PowLoc.bot <> (PowLoc.meet set buf_in_scc) then 
            { feat with init_index = true }
          else feat
      | _ -> feat) feat entry_prev
  in
  if (List.length entry_prev = 1) && IntraCfg.is_entry (List.hd entry_prev) then {feat with init_index = true }
  else feat
 
let add_diff_array_access cfg scc feat = 
  let qs = List.fold_left (fun qs node ->
      let cmd = IntraCfg.find_cmd node cfg in
      qs@(AlarmExp.collect cmd)) [] scc in
  let set = List.fold_left (fun set q ->
        match q with 
          AlarmExp.ArrayExp (arr, i, _)
        | AlarmExp.DerefExp (BinOp (_, Lval arr, i, _), _) ->
            BatSet.add arr set
        | _ -> set) BatSet.empty qs in
    { feat with diff_array_access = float_of_int (BatSet.cardinal set) }

let inc_itself_by_one (lv,e) = 
  match lv,e with
  | (Var x, NoOffset), (BinOp (PlusA, Lval (Var y,NoOffset),Const (CInt64 (i,_,_)),_)) 
     when x.vname = y.vname && Cil.i64_to_int i = 1 -> true
  | _ -> false

let add_idx_pp cfg scc feat = 
  let set = List.fold_left (fun set node ->
      let cmd = IntraCfg.find_cmd node cfg in
      match cmd with 
        IntraCfg.Cmd.Cset (lv,e,_) -> 
          if inc_itself_by_one (lv, e) then 
            BatSet.add lv set
          else set
      | _ -> set) BatSet.empty scc 
  in
    { feat with idx_pp = float_of_int (BatSet.cardinal set) }

let add_ptr_pp cfg scc feat = 
  let set = List.fold_left (fun set node ->
      let cmd = IntraCfg.find_cmd node cfg in
      match cmd with 
        IntraCfg.Cmd.Cset (lv,e,_) -> 
          if incptr_itself_by_one (lv, e) then 
            BatSet.add lv set
          else set
      | _ -> set) BatSet.empty scc 
  in
    { feat with ptr_pp = float_of_int (BatSet.cardinal set) }
  
module G = Graph.Persistent.Digraph.ConcreteBidirectional(IntraCfg.Node)
module Scc = Graph.Components.Make(G)

let rec get_nested_scc cfg scc_list =
  List.fold_left (fun scc_list scc ->
    if List.length scc > 1 then
      let entries = IntraCfg.fold_edges (fun src dst set ->
        if not (List.mem src scc) && List.mem dst scc then BatSet.add dst set
        else set) cfg BatSet.empty 
      in
      let subgraph = List.fold_left (fun g n -> 
        let succ = IntraCfg.succ n cfg in
        let succ = List.filter (fun x -> List.mem x scc) succ in
        List.fold_left (fun g s -> G.add_edge g n s) g succ) G.empty scc
      in 
      let entry = BatSet.choose entries in
      let pred = try G.pred subgraph entry with _ -> prerr_endline "not found"; exit(0) in
      let subgraph = List.fold_left (fun g p -> G.remove_edge g p entry) subgraph pred in
      let sub_scc = Scc.scc_list subgraph in
      let sub_scc = get_nested_scc cfg sub_scc in
      scc_list@sub_scc
    else scc_list
    ) scc_list scc_list          

let extract global cfg loop_info trset = 
  let scc_list = IntraCfg.get_scc_list cfg in
  let scc_list = get_nested_scc cfg scc_list in
  List.fold_left (fun trset scc ->
    if List.length scc = 1 then trset
    else
    let branches = List.filter (fun n -> 
                let succ = IntraCfg.succ n cfg in
                (List.length succ = 2) &&
                (List.exists (fun s -> not (List.mem s scc)) succ)) scc 
    in
    let loopid = try BatMap.find (IntraCfg.Node.id (List.hd branches)) loop_info with _ -> "unknown_loop" in
    let conds = List.map (fun head -> InterCfg.Node.make (IntraCfg.get_pid cfg) (List.hd (IntraCfg.succ head cfg))) branches in
    let feature = 
      (List.fold_left (fun feat cond_node ->
        let cond_exp = CilHelper.remove_cast (get_cond_exp cond_node cfg) in
        feat  
        |> add_null cond_exp cfg 
        |> add_constant cond_exp cfg 
        |> add_conjunction cond_node cfg 
        |> add_array_content global cond_exp cfg 
        |> add_prune_cond global cond_node 
        |> add_extern global cond_node
        |> add_gvar global cond_node 
        |> add_finite_itv global cond_node 
        |> add_close_left_arr_size_offset global cond_node cfg
        |> add_cstring global cond_node cfg
        )
      empty_feature conds)
      |> add_index conds cfg scc 
      |> add_out_index conds cfg scc 
      |> add_init global conds cfg scc 
      |> add_exits conds 
      |> add_scc_size scc 
      |> add_points_to global conds
      |> add_diff_array_access cfg scc 
      |> add_idx_pp cfg scc
      |> add_ptr_pp cfg scc
    in
    BatMap.add loopid feature trset
    ) trset scc_list

let rec generate_loop_info loopid stmts loop_info = 
  List.fold_left (fun info stmt ->
    match stmt.skind with 
      Loop (blk, loc, _, _) -> generate_loop_info loc blk.bstmts info
    | If (_, tb, fb, _) ->
        (BatMap.add stmt.sid (CilHelper.s_location loopid) info)
        |> generate_loop_info loopid tb.bstmts |> generate_loop_info loopid fb.bstmts
    | Block blk -> generate_loop_info loopid blk.bstmts info
    | Switch (_, blk, _, _) -> generate_loop_info loopid blk.bstmts info
    | _ -> BatMap.add stmt.sid (CilHelper.s_location loopid) info) loop_info stmts

let normalize trset = 
  let max_exit = 
    BatMap.fold (fun v max -> if v.exits > max then v.exits else max) trset 0.0
  in
  let max_scc = 
    BatMap.fold (fun v max -> if v.scc_size > max then v.scc_size else max) trset 0.0
  in
  let max_points_to = 
    BatMap.fold (fun v max -> if v.points_to > max then v.points_to else max) trset 0.0
  in
  let max_diff_array_access = 
    BatMap.fold (fun v max -> if v.diff_array_access > max then v.diff_array_access else max) trset 0.0
  in
  let max_idx_pp = 
    BatMap.fold (fun v max -> if v.idx_pp > max then v.idx_pp else max) trset 0.0
  in
  BatMap.map (fun feat -> 
    { feat with 
      exits = (feat.exits /. max_exit); 
      scc_size = feat.scc_size /. max_scc;
      points_to = feat.points_to /. max_points_to; 
      diff_array_access = if feat.diff_array_access = 0.0 then 0.0 else feat.diff_array_access /. max_diff_array_access;
      idx_pp = if feat.idx_pp = 0.0 then 0.0 else feat.idx_pp /. max_idx_pp;
      }) trset 

let extract_feature : Global.t -> data
= fun global ->
  let trset = Cil.foldGlobals global.file (fun trset glob ->
    match glob with 
      Cil.GFun (fd, _) ->
        (try 
          let loop_info = generate_loop_info Cil.locUnknown fd.sbody.bstmts BatMap.empty in
          let cfg = InterCfg.cfgof global.icfg fd.svar.vname in
          extract global cfg loop_info trset
        with _ -> trset)
    | _ -> trset) BatMap.empty 
  in
  if !Options.opt_debug then 
    (prerr_endline "== features for loop ==";
    BatMap.iter (fun k v -> prerr_endline (k^"\n"^(string_of_feature v))) trset);
  normalize trset

class loopRemoveVisitor (loops: string BatSet.t) = object(self)
  inherit nopCilVisitor
  method vstmt (s: Cil.stmt) = 
    match s.skind with
      Loop (blk,loc,_,_) when BatSet.mem (CilHelper.s_location loc) loops ->
        Cil.ChangeDoChildrenPost (Cil.mkStmt (Cil.Block blk), id)
    | _ -> Cil.DoChildren
end

let print_feature : data -> unit
= fun data -> 
  string_of_trset data |> print_string

let get_harmless_loops : Global.t -> loop BatSet.t
= fun global -> 
  if !Options.opt_bugfinder < 1 then BatSet.empty
  else
    let data = extract_feature global in
    let sparrow_bin_path = Unix.getenv "SPARROW_BIN_PATH" in
    let sparrow_data_path = Unix.getenv "SPARROW_DATA_PATH" in
    let py = Lymp.init ~exec:"python2" sparrow_bin_path in
    let py_module = Lymp.get_module py "harmless_unsoundness" in
    let classifier = Lymp.Pyref (Lymp.get_ref py_module "load" [Lymp.Pystr (sparrow_data_path ^ "/harmless_loop_clf")]) in
    let set = BatMap.foldi (fun l fvec loops -> 
        let vec = feature_vector_of fvec in
        let vec = Lymp.Pylist (List.map (fun x -> Lymp.Pyfloat x) vec) in 
        let b = Lymp.get_bool py_module "is_harmless" [classifier; vec] in
        if b then BatSet.add l loops else loops
        ) data BatSet.empty
    in
    Lymp.close py;
    set

let dissolve : Global.t -> bool
= fun global ->
  let target_loops = BatSet.union (get_harmless_loops global) !Options.opt_unsound_loop in
  let vis = new loopRemoveVisitor target_loops in
  ignore(Cil.visitCilFile vis global.file);
  not (BatSet.is_empty target_loops)
