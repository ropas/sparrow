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
open IntraCfg
open InterCfg
open OctDom
open OctImpactDom
open BatTuple

module Dom = OctImpactDom.Mem
module Spec = Spec.Make(Dom)

let pack = OctImpactDom.pack

let lookup : Mem.t -> AbsOct.t
= fun mem ->
  Mem.find pack mem

let can_strong_update : update_mode -> Global.t -> Proc.t -> bool
= fun mode global pid ->
  match mode with
  | Weak -> false
  | Strong -> not (Global.is_rec pid global)
    
let can_strong_update_octloc : update_mode -> Global.t -> PowOctLoc.t -> bool
= fun mode global lvs ->
  match mode,lvs with
  | Weak, _ -> false
  | Strong, lvs' ->
    if PowOctLoc.cardinal lvs' = 1 then
      match PowOctLoc.choose lvs' with 
        OctLoc.Loc lv -> Loc.is_gvar lv || (Loc.is_lvar lv && not (Global.is_rec (Loc.get_proc lv) global))
      | OctLoc.Size _ -> true
    else false

let update : update_mode -> Global.t -> Proc.t -> AbsOct.t -> Mem.t -> Mem.t  
= fun mode global pid v mem -> 
  if can_strong_update mode global pid then Mem.add pack v mem
  else Mem.weak_add pack v mem 

let rec set : update_mode -> Global.t -> ItvDom.Mem.t -> Proc.t -> PowOctLoc.t 
  -> Cil.exp -> Dom.t -> Dom.t
= fun mode global ptrmem pid lv_set e mem ->
  let o = lookup mem in
  if try Cil.isIntegralType (Cil.typeOf e) with _ -> false then 
    match e |> CilHelper.remove_cast |> CilHelper.remove_coeff with 
      Const _ ->
        lv_set
        |> (fun x -> 
              if can_strong_update_octloc mode global lv_set then
                PowOctLoc.fold AbsOct.set_const x o
              else 
                PowOctLoc.fold AbsOct.weak_set_const x o)
        |> (fun o -> update mode global pid o mem)
    | Lval lval -> 
        ItvSem.eval_lv pid lval ptrmem
        |> PowOctLoc.of_locs
        |> (fun rv_set ->
              if can_strong_update_octloc mode global lv_set &&
                 can_strong_update_octloc mode global rv_set
              then
                PowOctLoc.fold2 AbsOct.set_variable lv_set rv_set o
              else 
                PowOctLoc.fold2 AbsOct.weak_set_variable lv_set rv_set o)
        |> (fun o -> update mode global pid o mem)
      (* x = y + c *)
    | BinOp (bop, Lval lval, Const (CInt64 (i, _, _)), _) 
    | BinOp (bop, Const (CInt64 (i, _, _)), Lval lval, _) 
      (* heuristic: forget the relationship only if x = x + 1 *)
      when (bop = PlusA && (Cil.i64_to_int i) = 1) || (bop = MinusA && (Cil.i64_to_int i) = -1) -> 
        ItvSem.eval_lv pid lval ptrmem
        |> PowOctLoc.of_locs
        |> (fun rv_set ->
              if PowOctLoc.meet lv_set rv_set |> PowOctLoc.is_empty then
                set mode global ptrmem pid lv_set (Lval lval) mem 
              else 
                forget mode global pid lv_set mem)
    | BinOp (bop, Lval lval, Const _, _) 
    | BinOp (bop, Const _, Lval lval, _) 
      when bop = PlusA || bop = MinusA -> 
        set mode global ptrmem pid lv_set (Lval lval) mem 
    | _ -> forget mode global pid lv_set mem 
  else mem

and forget : update_mode -> Global.t -> Proc.t -> PowOctLoc.t -> Dom.t -> Dom.t
= fun mode global pid lv_set mem ->
  let o = lookup mem in
  lv_set 
  |> (fun l -> PowOctLoc.fold AbsOct.forget l o)
  |> (fun o -> update mode global pid o mem)

let alloc : update_mode -> Global.t -> ItvDom.Mem.t -> Proc.t -> PowOctLoc.t 
  -> PowOctLoc.t -> Cil.exp -> Dom.t -> Dom.t 
= fun mode global ptrmem pid lv_set ptrs e mem -> 
  set mode global ptrmem pid ptrs e mem

let rec prune : update_mode -> Global.t -> ItvDom.Mem.t -> Proc.t -> Cil.exp 
  -> Dom.t -> Dom.t
= fun mode global ptrmem pid exp mem -> 
  let o = lookup mem in 
  match exp |> CilHelper.remove_cast |> CilHelper.remove_coeff |> CilHelper.make_cond_simple with
    None -> mem
  | Some (Cil.BinOp (bop, Cil.Lval x, Cil.Lval y, _)) 
    when bop = Lt || bop = Le || bop = Eq ->
      (x, y) 
      |> BatTuple.Tuple2.mapn (fun lv -> ItvSem.eval_lv pid lv ptrmem)
      |> BatTuple.Tuple2.mapn PowOctLoc.of_locs
      |> (fun (lv_set, rv_set) -> 
            if can_strong_update_octloc mode global lv_set &&
               can_strong_update_octloc mode global rv_set
            then
              PowOctLoc.fold2 AbsOct.assume lv_set rv_set o
            else 
              PowOctLoc.fold2 AbsOct.weak_assume lv_set rv_set o)
      |> (fun o -> update mode global pid o mem)
  | Some (Cil.BinOp (bop, Cil.Lval x, Cil.BinOp (bop2, Cil.Lval y, Cil.Const _, _), t))
  | Some (Cil.BinOp (bop, Cil.Lval x, Cil.BinOp (bop2, Cil.Const _, Cil.Lval y, _), t))
    when (bop = Lt || bop = Le || bop = Eq) && (bop2 = PlusA || bop2 = MinusA) -> 
      prune mode global ptrmem pid (Cil.BinOp (bop, Cil.Lval x, Cil.Lval y, t)) mem
  | Some (Cil.BinOp (bop, Cil.Lval x, Cil.Lval y, t)) 
    when bop = Gt || bop = Ge ->
      prune mode global ptrmem pid (Cil.BinOp (CilHelper.rev_binop bop, Cil.Lval y, Cil.Lval x, t)) mem
  | _ -> mem

let model_realloc mode global node pid lvo exps ptrmem (mem, global) =
  match lvo, exps with
    (Some l, _::size::_) ->
      let lv = ItvSem.eval_lv pid l ptrmem |> PowOctLoc.of_locs in
      let ptrs = ItvSem.eval_array_alloc node size false ptrmem |> ItvDom.Val.allocsites_of_val |> PowOctLoc.of_sizes in
      alloc mode global ptrmem pid lv ptrs size mem
  | _ -> mem

let model_calloc mode global node pid lvo exps ptrmem (mem, global) =
  match lvo, exps with
    (Some l, size::_) -> 
      let lv = ItvSem.eval_lv pid l ptrmem |> PowOctLoc.of_locs in
      let ptrs = ItvSem.eval_array_alloc node size false ptrmem |> ItvDom.Val.allocsites_of_val |> PowOctLoc.of_sizes in
      alloc mode global ptrmem pid lv ptrs size mem
  | _ -> mem

let model_input mode pid lvo ptrmem (mem,global) =
  match lvo with 
    Some lv -> 
      let size = Allocsite.allocsite_of_ext None |> OctLoc.of_size |> PowOctLoc.singleton in
      forget mode global pid size mem
  | _ -> mem

let model_strdup mode pid node lvo exps ptrmem (mem,global) = 
  match (lvo, exps) with
    (Some lv, str::_) ->
      let o = lookup mem in
      let lv_set = Allocsite.allocsite_of_node node |> OctLoc.of_size |> PowOctLoc.singleton in
      let rv_set = ItvSem.eval pid str ptrmem |> ItvDom.Val.allocsites_of_val |>  PowOctLoc.of_sizes in
      let o = 
            if can_strong_update_octloc mode global lv_set &&
               can_strong_update_octloc mode global rv_set
            then
              PowOctLoc.fold2 AbsOct.set_variable lv_set rv_set o
            else 
              PowOctLoc.fold2 AbsOct.weak_set_variable lv_set rv_set o
      in
      update mode global pid o mem
  | (_, _) -> mem
     

let sparrow_print : Proc.t -> Cil.exp list -> Dom.t -> Cil.location -> unit
= fun pid exps mem loc ->  
  lookup mem
  |> AbsOct.to_string 
  |> (fun x -> "sparrow_print (" ^ CilHelper.s_location loc ^ ") : \n" ^ x)
  |> prerr_endline

let sparrow_arg mode pid exps ptrmem (mem,global) = 
  match exps with
    (Cil.Lval argc)::(Cil.Lval argv)::_ ->
      let lv = ItvSem.eval_lv pid argv ptrmem |> PowOctLoc.of_locs in
      let argc_lv = ItvSem.eval_lv pid argc ptrmem |> PowOctLoc.of_locs in
      let argv_a = Allocsite.allocsite_of_ext (Some "argv") |> OctLoc.of_size |> PowOctLoc.singleton in
      let arg_a = Allocsite.allocsite_of_ext (Some "arg") |> OctLoc.of_size |> PowOctLoc.singleton in 
      mem
      |> forget mode global pid argc_lv
      |> alloc mode global ptrmem pid lv argv_a (Cil.Lval argc)
      |> forget mode global pid arg_a 
  | _ -> mem

let model_strlen mode pid lvo exps ptrmem (mem, global) =
  match lvo, exps with 
  | (Some lv, (str::_)) ->  
    let rv_set = ItvSem.eval pid str ptrmem |> ItvDom.Val.allocsites_of_val |> PowOctLoc.of_sizes in
    let lv_set = ItvSem.eval_lv pid lv ptrmem |> PowOctLoc.of_locs in
    let mem = forget mode global pid lv_set mem in 
    let o = lookup mem in
    let o =
        (if can_strong_update_octloc mode global lv_set &&
           can_strong_update_octloc mode global rv_set
        then
          PowOctLoc.fold2 AbsOct.assume lv_set rv_set o
        else 
          PowOctLoc.fold2 AbsOct.weak_assume lv_set rv_set o)
    in
    update mode global pid o mem
  | _ -> mem

let handle_undefined_functions mode node pid (lvo,f,exps) ptrmem (mem,global) loc =
  match f.vname with
  | "sparrow_print" -> sparrow_print pid exps mem loc; mem
  | "sparrow_arg" -> sparrow_arg mode pid exps ptrmem (mem,global)
  | "strlen" -> model_strlen mode pid lvo exps ptrmem (mem, global) 
  | "getenv" -> model_input mode pid lvo ptrmem (mem,global)
  | "strdup" -> model_strdup mode pid node lvo exps ptrmem (mem,global)
  | "realloc" -> model_realloc mode global node pid lvo exps ptrmem (mem, global) 
  | "calloc" -> model_calloc mode global node pid lvo exps ptrmem (mem, global) 
  | _ ->
    (match lvo with 
       None -> mem
     | Some lv -> 
       (match (Cil.unrollTypeDeep (Cil.typeOfLval lv)) with 
          Cil.TInt (_, _) | Cil.TFloat (_, _) ->          
            let lv = ItvSem.eval_lv pid lv ptrmem in
            let oct_lv = PowOctLoc.of_locs lv in 
            forget mode global pid oct_lv mem
        | _ -> 
            let size = Allocsite.allocsite_of_ext (Some f.vname) |> OctLoc.of_size |> PowOctLoc.singleton  in
            let loc  = Allocsite.allocsite_of_ext (Some f.vname) |> Loc.of_allocsite |> OctLoc.of_loc |> PowOctLoc.singleton  in
            mem
            |> forget mode global pid size 
            |> forget mode global pid loc))

      

let binding : update_mode -> Global.t -> ItvDom.Mem.t -> Proc.t -> (Loc.t list) BatSet.t -> Cil.exp list -> Dom.t -> Dom.t
= fun mode global ptrmem pid paramset args mem ->
  let rec adjust params args new_params new_args =
    match (params, args) with
      (h1::t1, h2::t2) -> adjust t1 t2 (h1::new_params) (h2::new_args)
    | ([], _) | (_, []) -> (new_params, new_args)
  in
  let mode = if BatSet.cardinal paramset > 1 then Weak else mode in
  BatSet.fold (fun params mem ->
      let (params, args) = adjust params args [] [] in
      List.fold_left2 (fun mem param arg -> 
        let param = PowLoc.singleton param |> PowOctLoc.of_locs in
        set mode global ptrmem pid param arg mem) mem params args
  ) paramset mem

let rec run_cmd mode node cmd ptrmem (mem,global) =
  let pid = Node.get_pid node in
(*  prerr_endline (IntraCfg.Cmd.to_string cmd);*)
  match cmd with
  | IntraCfg.Cmd.Cset (l, e, _) ->
      let lv = ItvSem.eval_lv pid l ptrmem |> PowOctLoc.of_locs in 
      set mode global ptrmem pid lv e mem
  | IntraCfg.Cmd.Cexternal (l, _) ->
      let lv = ItvSem.eval_lv pid l ptrmem in
      let oct_lv = PowOctLoc.of_locs lv in
      let (arr_val, arr_size) = 
        ItvDom.Mem.lookup lv ptrmem
        |> ItvDom.Val.array_of_val 
        |> tuple
        |> Tuple2.map ArrayBlk.pow_loc_of_array ArrayBlk.allocsites_of_array
        |> Tuple2.map PowOctLoc.of_locs PowOctLoc.of_sizes 
      in
      mem 
      |> forget mode global pid oct_lv
      |> forget mode global pid arr_val
      |> forget mode global pid arr_size
  | IntraCfg.Cmd.Calloc (l, IntraCfg.Cmd.Array e, is_static, _) ->  
      let lv = ItvSem.eval_lv pid l ptrmem |> PowOctLoc.of_locs in
      let ptrs = ItvSem.eval_array_alloc node e is_static ptrmem |> ItvDom.Val.allocsites_of_val |> PowOctLoc.of_sizes in
      alloc mode global ptrmem pid lv ptrs e mem
  | IntraCfg.Cmd.Csalloc (l,s,_) -> 
      let lv = ItvSem.eval_lv pid l ptrmem |> PowOctLoc.of_locs in 
      let ptrs = ItvSem.eval_string_alloc node s ptrmem |> ItvDom.Val.allocsites_of_val |> PowOctLoc.of_sizes in
      let e = Cil.integer (String.length s + 1) in
      alloc mode global ptrmem pid lv ptrs e mem
  | IntraCfg.Cmd.Cassume (e, _) ->
      prune mode global ptrmem pid e mem
  | IntraCfg.Cmd.Ccall (lvo, Cil.Lval (Cil.Var f, Cil.NoOffset), arg_exps, loc)
    when Global.is_undef f.vname global -> (* undefined library functions *)
      let _ = lookup mem in (* for inspection *)
      handle_undefined_functions mode node pid (lvo,f,arg_exps) ptrmem (mem,global) loc
  | IntraCfg.Cmd.Ccall (lvo, f, arg_exps, _) ->
      let fs = ItvDom.Val.pow_proc_of_val (ItvSem.eval pid f ptrmem) in
      if PowProc.eq fs PowProc.bot then mem
      else
        let arg_lvars_of_proc f acc =
          let args = InterCfg.argsof global.icfg f in
          let lvars = List.map (fun x -> Loc.of_lvar f x.Cil.vname x.Cil.vtype) args in
          BatSet.add lvars acc in
        let arg_lvars_set = PowProc.fold arg_lvars_of_proc fs BatSet.empty in
        binding mode global ptrmem pid arg_lvars_set arg_exps mem
  | IntraCfg.Cmd.Creturn (Some e, _) -> 
      let ret_locs = Dump.find (InterCfg.Node.get_pid node) global.dump |> PowOctLoc.of_locs in
      set mode global ptrmem pid ret_locs e mem
  | IntraCfg.Cmd.Cskip when InterCfg.start_node = node -> 
      update mode global pid AbsOct.top mem
  | _ -> mem

let run : update_mode -> Spec.t -> Node.t -> Dom.t * Global.t -> Dom.t * Global.t
= fun mode spec node (mem, global) ->
  let ptrmem = ItvDom.Table.find node spec.Spec.ptrinfo in
  let mem = run_cmd mode node (InterCfg.cmdof global.icfg node) ptrmem (mem,global) in
  (mem, global)

let initial locset = Mem.init locset
