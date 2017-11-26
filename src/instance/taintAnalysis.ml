open AlarmExp
open BasicDom
open Cil
open Global
open Report
open Vocab

module Analysis = SparseAnalysis.Make(TaintSem)
module Table = Analysis.Table
module Spec = Analysis.Spec
module Mem = TaintDom.Mem

let marshal_in global =
  let filename = Filename.basename global.file.fileName in
  let global = MarshalManager.input (filename ^ ".taint.global") in
  let input = MarshalManager.input (filename ^ ".taint.input") in
  let output = MarshalManager.input (filename ^ ".taint.output") in
  (global,input,output)

let marshal_out (global,input,output) =
  let filename = Filename.basename global.file.fileName in
  MarshalManager.output (filename ^ ".taint.global") global;
  MarshalManager.output (filename ^ ".taint.input") input;
  MarshalManager.output (filename ^ ".taint.output") output;
  (global,input,output)

let inspect_aexp node aexp itvmem mem queries =
  match aexp with
  | AllocSize (e,loc) ->
    let pid = InterCfg.Node.get_pid node in
    let size_itv = ItvSem.eval pid e itvmem |> ItvDom.Val.itv_of_val in
    let taint = TaintSem.eval pid e itvmem mem |> TaintDom.Val.user_input in
    TaintDom.UserInput.fold (fun (src_node, src_loc) queries ->
        let size_ovfl = TaintSem.eval pid e itvmem mem |> TaintDom.Val.int_overflow |> TaintDom.IntOverflow.is_bot |> not in
        let status = if size_ovfl then UnProven else Proven in
        let desc = "size = " ^ Itv.to_string size_itv
                   ^ ", source = " ^ Node.to_string src_node ^ " @"
                   ^ CilHelper.s_location src_loc
        in
        { node; exp = aexp; loc; allocsite = None; status; desc; src = Some (src_node, src_loc) } :: queries) taint queries
  | _ -> queries

let inspect_alarm global spec inputof =
  let nodes = InterCfg.nodesof global.icfg in
  let total = List.length nodes in
  list_fold (fun node (qs,k) ->
    prerr_progressbar ~itv:1000 k total;
    let ptrmem = ItvDom.Table.find node spec.Spec.ptrinfo in
    let mem = Table.find node inputof in
    let cmd = InterCfg.cmdof global.icfg node in
    let aexps = AlarmExp.collect cmd in
    let qs = list_fold (fun aexp ->
      if ptrmem = ItvDom.Mem.bot then id (* dead code *)
      else inspect_aexp node aexp ptrmem mem) aexps qs
    in
    (qs, k+1)
  ) nodes ([],0)
  |> fst

let get_locset mem =
  ItvDom.Mem.foldi (fun l v locset ->
    locset
    |> PowLoc.add l
    |> PowLoc.union (ItvDom.Val.pow_loc_of_val v)
    |> BatSet.fold (fun a -> PowLoc.add (Loc.of_allocsite a)) (ItvDom.Val.allocsites_of_val v)
  ) mem PowLoc.empty

let make_top_mem locset =
  PowLoc.fold (fun l mem ->
      Mem.add l TaintDom.Val.top mem) locset Mem.bot

let do_analysis (global, itvinputof) =
  let global = { global with table = itvinputof } in
  let locset = get_locset global.mem in
  let spec = { Spec.empty with
               Spec.locset; Spec.locset_fs = locset;
               Spec.premem = make_top_mem locset;
               Spec.ptrinfo = itvinputof } in
  (* NOTE: fully flow-sensitive taint analysis *)
  let _ = Options.pfs := 100 in
  cond !Options.marshal_in marshal_in (Analysis.perform spec) global
  |> opt !Options.marshal_out marshal_out
  |> StepManager.stepf true "Generate Alarm Report"
    (fun (global,inputof,outputof) ->
       (global,inputof,outputof,inspect_alarm global spec inputof))
