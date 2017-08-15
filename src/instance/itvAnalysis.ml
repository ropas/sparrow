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
open Graph
open Cil
open Global
open BasicDom
open Vocab
open Frontend
open IntraCfg
open ItvDom
open ArrayBlk
open AlarmExp 
open Report 

module Analysis = SparseAnalysis.Make(ItvSem)
module Table = Analysis.Table
module Spec = Analysis.Spec

let print_abslocs_info locs = 
  let lvars = BatSet.filter Loc.is_lvar locs in
  let gvars = BatSet.filter Loc.is_gvar locs in
  let allocsites = BatSet.filter Loc.is_allocsite locs in
  let fields = BatSet.filter Loc.is_field locs in
    prerr_endline ("#abslocs    : " ^ i2s (BatSet.cardinal locs));
    prerr_endline ("#lvars      : " ^ i2s (BatSet.cardinal lvars));
    prerr_endline ("#gvars      : " ^ i2s (BatSet.cardinal gvars));
    prerr_endline ("#allocsites : " ^ i2s (BatSet.cardinal allocsites));
    prerr_endline ("#fields     : " ^ i2s (BatSet.cardinal fields))

(* **************** *
 * Alarm Inspection *
 * **************** *)
let ignore_alarm a arr offset =
  (!Options.bugfinder >= 1 
    && (Allocsite.is_string_allocsite a
       || arr.ArrInfo.size = Itv.top
       || arr.ArrInfo.size = Itv.one
       || offset = Itv.top && arr.ArrInfo.size = Itv.nat
       || offset = Itv.zero))
  || (!Options.bugfinder >= 2
      && not (Itv.is_const arr.ArrInfo.size))
  || (!Options.bugfinder >= 3
       && (offset = Itv.top 
          || Itv.meet arr.ArrInfo.size Itv.zero <> Itv.bot
          || (offset = Itv.top && arr.ArrInfo.offset <> Itv.top)))


let check_bo v1 v2opt : (status * Allocsite.t option * string) list = 
  let arr = Val.array_of_val v1 in
  if ArrayBlk.eq arr ArrayBlk.bot then [(BotAlarm, None, "Array is Bot")] else
    ArrayBlk.foldi (fun a arr lst ->
      let offset = 
        match v2opt with
        | None -> arr.ArrInfo.offset
        | Some v2 -> Itv.plus arr.ArrInfo.offset (Val.itv_of_val v2) in
      let status =
        try 
          if Itv.is_bot offset || Itv.is_bot arr.ArrInfo.size then BotAlarm
          else if ignore_alarm a arr offset then Proven
          else 
            let (ol, ou) = (Itv.lower offset, Itv.upper offset) in
            let sl = Itv.lower arr.ArrInfo.size in
            if ou >= sl || ol < 0 then UnProven
            else Proven
        with _ -> UnProven
      in
      (status, Some a, string_of_alarminfo offset arr.ArrInfo.size)::lst
    ) arr []

let check_nd v1 : (status * Allocsite.t option * string) list = 
  let ploc = Val.pow_loc_of_val v1 in
  if PowLoc.eq ploc PowLoc.bot then [(BotAlarm, None, "PowLoc is Bot")] else
    if PowLoc.mem Loc.null ploc then 
      [(UnProven, None, "Null Dereference")]
    else [(Proven, None, "")]

let inspect_aexp_bo : InterCfg.node -> AlarmExp.t -> Mem.t -> query list -> query list
=fun node aexp mem queries ->
  (match aexp with
    | ArrayExp (lv,e,loc) ->
        let v1 = Mem.lookup (ItvSem.eval_lv (InterCfg.Node.get_pid node) lv mem) mem in
        let v2 = ItvSem.eval (InterCfg.Node.get_pid node) e mem in
        let lst = check_bo v1 (Some v2) in
        List.map (fun (status,a,desc) -> 
          { node = node; exp = aexp; loc = loc; allocsite = a; 
          status = status; desc = desc }) lst
    | DerefExp (e,loc) ->
        let v = ItvSem.eval (InterCfg.Node.get_pid node) e mem in
        let lst = check_bo v None in 
          if Val.eq Val.bot v then 
            List.map (fun (status,a,desc) -> 
              { node = node; exp = aexp; loc = loc; allocsite = a; 
              status = status; desc = desc }) lst
          else 
            List.map (fun (status,a,desc) -> 
              if status = BotAlarm
              then { node = node; exp = aexp; loc = loc; status = Proven; allocsite = a; 
                     desc = "valid pointer dereference" }
              else { node = node; exp = aexp; loc = loc; status = status; allocsite = a; 
                     desc = desc }) lst
    | Strcpy (e1, e2, loc) ->
        let v1 = ItvSem.eval (InterCfg.Node.get_pid node) e1 mem in
        let v2 = ItvSem.eval (InterCfg.Node.get_pid node) e2 mem in
        let v2 = Val.of_itv (ArrayBlk.nullof (Val.array_of_val v2)) in 
        let lst = check_bo v1 (Some v2) in
        List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = a; 
            status = status; desc = desc }) lst
    | Strcat (e1, e2, loc) ->
        let v1 = ItvSem.eval (InterCfg.Node.get_pid node) e1 mem in
        let v2 = ItvSem.eval (InterCfg.Node.get_pid node) e2 mem in 
        let np1 = ArrayBlk.nullof (Val.array_of_val v1) in
        let np2 = ArrayBlk.nullof (Val.array_of_val v2) in
        let np = Val.of_itv (Itv.plus np1 np2) in
        let lst = check_bo v1 (Some np) in
        List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = a; 
            status = status; desc = desc }) lst
    | Strncpy (e1, e2, e3, loc) 
    | Memcpy (e1, e2, e3, loc) 
    | Memmove (e1, e2, e3, loc) ->
        let v1 = ItvSem.eval (InterCfg.Node.get_pid node) e1 mem in
        let v2 = ItvSem.eval (InterCfg.Node.get_pid node) e2 mem in
        let e3_1 = Cil.BinOp (Cil.MinusA, e3, Cil.mone, Cil.intType) in
        let v3 = ItvSem.eval (InterCfg.Node.get_pid node) e3_1 mem in
        let lst1 = check_bo v1 (Some v3) in
        let lst2 = check_bo v2 (Some v3) in
        List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = a; 
            status = status; desc = desc }) (lst1@lst2)
    | _ -> []) @ queries

let inspect_aexp_nd : InterCfg.node -> AlarmExp.t -> Mem.t -> query list -> query list
=fun node aexp mem queries ->
  (match aexp with
  | DerefExp (e,loc) ->
    let v = ItvSem.eval (InterCfg.Node.get_pid node) e mem in
    let lst = check_nd v in 
      if Val.eq Val.bot v then 
        List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = a; 
          status = status; desc = desc }) lst
      else 
        List.map (fun (status,a,desc) -> 
          if status = BotAlarm
          then { node = node; exp = aexp; loc = loc; status = Proven; allocsite = a; 
            desc = "valid pointer dereference" }
          else { node = node; exp = aexp; loc = loc; status = status; allocsite = a; 
            desc = desc }) lst
  | _ -> []) @ queries

let check_dz v = 
  let v = Val.itv_of_val v in
  if Itv.le Itv.zero v then 
    [(UnProven, None, "Divide by "^Itv.to_string v)]
  else [(Proven, None, "")]

let inspect_aexp_dz : InterCfg.node -> AlarmExp.t -> Mem.t -> query list -> query list
= fun node aexp mem queries -> 
  (match aexp with 
      DivExp (_, e, loc) ->
      let v = ItvSem.eval (InterCfg.Node.get_pid node) e mem in
      let lst = check_dz v in 
        List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = None; 
          status = status; desc = desc }) lst
  | _ -> []) @ queries

let machine_gen_code : query -> bool 
= fun q -> 
  (* yacc-generated code *)
  Filename.check_suffix q.loc.Cil.file ".y" || Filename.check_suffix q.loc.Cil.file ".yy.c" ||
  Filename.check_suffix q.loc.Cil.file ".simple" ||
  (* sparrow-generated code *)
  InterCfg.Node.get_pid q.node = InterCfg.global_proc 
  
let rec unsound_exp : Cil.exp -> bool
= fun e ->
  match e with 
  | Cil.BinOp (Cil.PlusPI, Cil.Lval (Cil.Mem _, _), _, _) -> true
  | Cil.BinOp (b, _, _, _) when b = Mod || b = Cil.Shiftlt || b = Shiftrt || b = BAnd 
      || b = BOr || b = BXor || b = LAnd || b = LOr -> true
  | Cil.BinOp (bop, Cil.Lval (Cil.Var _, _), Cil.Lval (Cil.Var _, _), _) 
    when bop = Cil.PlusA || bop = Cil.MinusA -> true 
  | Cil.BinOp (_, e1, e2, _) -> (unsound_exp e1) || (unsound_exp e2)
  | Cil.CastE (_, e) -> unsound_exp e
  | Cil.Lval lv -> unsound_lv lv
  | _ -> false

and unsound_lv : Cil.lval -> bool = function 
  | (_, Cil.Index _) -> true
  | (Cil.Var v, _) -> is_global_integer v || is_union v.vtype || is_temp_integer v
  | (Cil.Mem _, Cil.NoOffset) -> true
  | (_, _) -> false
and is_global_integer v = v.vglob && Cil.isIntegralType v.vtype
and is_union typ = 
  match Cil.unrollTypeDeep typ with 
    Cil.TPtr (Cil.TComp (c, _), _) -> not c.cstruct
  | _ -> false
and is_temp_integer v = 
  !Options.bugfinder >= 2
  && (try String.sub v.vname 0 3 = "tmp" with _ -> false)
  && Cil.isIntegralType v.vtype

let unsound_aexp : AlarmExp.t -> bool = function 
  | ArrayExp (lv, e, _) -> unsound_exp e
  | DerefExp (e, _) -> unsound_exp e
  | _ -> false

let formal_param : Global.t -> query -> bool
= fun global q ->
  let cfg = InterCfg.cfgof global.icfg (InterCfg.Node.get_pid q.node) in
  let formals = IntraCfg.get_formals cfg |> List.map (fun x -> x.Cil.vname) in
  let rec find_exp = function
    | Cil.BinOp (_, e1, e2, _) -> (find_exp e1) || (find_exp e2)
    | Cil.CastE (_, e) -> find_exp e
    | Cil.Lval lv -> find_lv lv
    | _ -> false
  and find_lv = function 
    | (Cil.Var v, _) -> (List.mem v.vname formals) && Cil.isIntegralType v.vtype
    | (_, _) -> false
  in
  match q.exp with 
  | ArrayExp (_, e, _) | DerefExp (e, _) -> find_exp e
  | _ -> false
 
let unsound_filter : Global.t -> query list -> query list
= fun global ql ->
  let filtered = 
    List.filter (fun q -> 
      not (machine_gen_code q) 
      && not (unsound_aexp q.exp) 
(*     not (formal_param global q)*)) ql
  in
  let partition = 
    list_fold (fun q m ->
      let p_als = try BatMap.find (q.loc,q.node) m with _ -> [] in
        BatMap.add (q.loc,q.node) (q::p_als) m
    ) filtered BatMap.empty
  in
  BatMap.fold (fun ql result ->
      if List.length (Report.get ql UnProven) > 3 then 
        (List.map (fun q -> { q with status = Proven}) ql)@result
      else ql@result) partition []

let filter : query list -> status -> query list
= fun qs s -> List.filter (fun q -> q.status = s) qs

let generate : Global.t * Table.t * target -> query list
=fun (global,inputof,target) ->
  let nodes = InterCfg.nodesof global.icfg in
  let total = List.length nodes in
  list_fold (fun node (qs,k) ->
    prerr_progressbar ~itv:1000 k total;
    let mem = Table.find node inputof in
    let cmd = InterCfg.cmdof global.icfg node in
    let aexps = AlarmExp.collect cmd in 
    let qs = list_fold (fun aexp ->
      if mem = Mem.bot then id (* dead code *)
      else 
        match target with 
          BO -> inspect_aexp_bo node aexp mem
        | ND -> inspect_aexp_nd node aexp mem
        | DZ -> inspect_aexp_dz node aexp mem
      ) aexps qs
    in
    (qs, k+1)
  ) nodes ([],0)
  |> fst
  |> opt (!Options.bugfinder > 0) (unsound_filter global)

let generate_with_mem : Global.t * Mem.t * target -> query list
=fun (global,mem,target) ->
  let nodes = InterCfg.nodesof global.icfg in
    list_fold (fun node ->
      let cmd = InterCfg.cmdof global.icfg node in
      let aexps = AlarmExp.collect cmd in 
        if mem = Mem.bot then id (* dead code *)
        else 
          match target with 
            BO -> list_fold (fun aexp  -> inspect_aexp_bo node aexp mem) aexps 
          | ND -> list_fold (fun aexp  -> inspect_aexp_nd node aexp mem) aexps
          | DZ -> list_fold (fun aexp  -> inspect_aexp_dz node aexp mem) aexps
    ) nodes []

(* ********** *
 * Marshaling *
 * ********** *)

let marshal_in : Global.t -> Global.t * Table.t * Table.t
= fun global ->
  let filename = Filename.basename global.file.fileName in
  let global = MarshalManager.input (filename ^ ".itv.global") in
  let input = MarshalManager.input (filename ^ ".itv.input") in
  let output = MarshalManager.input (filename ^ ".itv.output") in
  (global,input,output)

let marshal_out : Global.t * Table.t * Table.t -> Global.t * Table.t * Table.t
= fun (global,input,output) ->
  let filename = Filename.basename global.file.fileName in
  MarshalManager.output (filename ^ ".itv.global") global;
  MarshalManager.output (filename ^ ".itv.input") input;
  MarshalManager.output (filename ^ ".itv.output") output;
  (global,input,output)

let inspect_alarm : Global.t -> Spec.t -> Table.t -> Report.query list
= fun global _ inputof ->
  (if !Options.bo then generate (global,inputof,Report.BO) else [])
  @ (if !Options.nd then generate (global,inputof,Report.ND) else [])
  @ (if !Options.dz then  generate (global,inputof,Report.DZ) else [])

let get_locset mem = 
  ItvDom.Mem.foldi (fun l v locset -> 
    locset
    |> PowLoc.add l
    |> PowLoc.union (Val.pow_loc_of_val v)
    |> BatSet.fold (fun a -> PowLoc.add (Loc.of_allocsite a)) (Val.allocsites_of_val v)
  ) mem PowLoc.empty 

let do_analysis : Global.t -> Global.t * Table.t * Table.t * Report.query list 
= fun global -> 
  let _ = prerr_memory_usage () in
  let locset = get_locset global.mem in
  let locset_fs = PartialFlowSensitivity.select global locset in
  let unsound_lib = UnsoundLib.collect global in
  let unsound_update = (!Options.bugfinder >= 2) in
  let unsound_bitwise = (!Options.bugfinder >= 1) in
  let spec = { Spec.empty with 
    Spec.locset; Spec.locset_fs; premem = global.mem; Spec.unsound_lib; 
    Spec.unsound_update; Spec.unsound_bitwise; } in
  cond !Options.marshal_in marshal_in (Analysis.perform spec) global
  |> opt !Options.marshal_out marshal_out
  |> StepManager.stepf true "Generate Alarm Report" (fun (global,inputof,outputof) -> 
      (global,inputof,outputof,inspect_alarm global spec inputof))
