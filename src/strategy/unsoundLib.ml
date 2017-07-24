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
open AbsDom
open Vocab
open BasicDom
open ItvDom
open ItvSem

module AccessSem = 
struct
  include AccessSem.Make(ItvSem)
  let accessof_eval pid e mem = 
    Dom.init_access ();
    let _ = ItvSem.eval pid e mem in
    Dom.return_access ()
end
module AccessAnalysis = AccessAnalysis.Make(AccessSem)
module Access = ItvSem.Dom.Access

type lib = string

type feature = {
  constant : bool;              (* Whether the parameters contain constants or not *)
  return : bool;                (* Whether the lib has a return value or not *)
  return_int : bool;            (* Whether the return type is int or not *)
  from_string : bool;           (* Whether the function is declared in string.h or not *)
  inside_loop : bool;           (* Whether the function is called in a loop or not *)
  size : float;                 (* The (normalized) number of arguments *)
  update_param_in_loop : bool;  (* Whether a parameter are defined in a loop or not *)
  use_ret_in_loop : bool;       (* Whether the return value is used in a loop or not *)
  update_param_itself : bool;   (* Whether a parameter is update via the library call *)
  out_of_fun : bool;            (* Whether the return value escapes the caller *)
  gvar : bool;                  (* Whether a parameters points to a global variable *)
  extern : float;               (* Whether a parameters are determined by external inputs *)
  finite : float;               (* Whether a parameter have a finite interval value *)
  points_to : float;            (* The normalized number of abstract locations accessed in the arguments *)
  cstring : float;              (* The normalized number of string arguments *)
}

type data = (lib, feature) BatMap.t

let empty_feature = {
  constant = false;
  size = 0.0;
  return = false;
  return_int = false;
  extern = 0.0;
  gvar = false;
  from_string = false;
  inside_loop = false;
  update_param_in_loop = false;
  use_ret_in_loop = false;
  update_param_itself = false;
  out_of_fun = false;
  finite = 0.0;
  points_to = 0.0;
  cstring = 0.0;
}

let string_of_feature f =
  "constant : "^(string_of_bool f.constant)^"\n"^
  "size : "^(string_of_float f.size)^"\n"^
  "return : "^(string_of_bool f.return)^"\n"^
  "return_int : "^(string_of_bool f.return_int)^"\n"^
  "extern : "^(string_of_float f.extern)^"\n"^
  "gvar : "^(string_of_bool f.constant)^"\n"^
  "from_string : "^(string_of_bool f.from_string)^"\n"^
  "inside_loop : "^(string_of_bool f.inside_loop)^"\n"^
  "update_param_in_loop : "^(string_of_bool f.update_param_in_loop)^"\n"^
  "use_ret_in_loop : "^(string_of_bool f.use_ret_in_loop)^"\n"^
  "update_param_itself : "^(string_of_bool f.update_param_itself)^"\n"^
  "out_of_fun : "^(string_of_bool f.out_of_fun)^"\n"^
  "finite : "^(string_of_float f.finite)^"\n"^
  "points_to : "^(string_of_float f.points_to)^"\n"^
  "cstring : "^(string_of_float f.cstring)^"\n"^
  "\n"

let b2is b = if b then "1" else "0"
let string_of_raw_feature f =
  (b2is f.constant)^"\t"^
  (string_of_float f.size)^"\t"^
  (b2is f.return)^"\t"^
  (b2is f.return_int)^"\t"^
  (string_of_float f.extern)^"\t"^
  (b2is f.gvar)^"\t"^
  (b2is f.from_string)^"\t"^
  (b2is f.inside_loop)^"\t"^
  (b2is f.update_param_in_loop)^"\t"^
  (b2is f.use_ret_in_loop)^"\t"^
  (b2is f.update_param_itself)^"\t"^
  (b2is f.out_of_fun)^"\t"^
  (string_of_float f.finite)^"\t"^
  (string_of_float f.points_to)^"\t"^
  (string_of_float f.cstring)^"\t"^
  "\t"

let float_of_bool b = if b then 1.0 else 0.0
let feature_vector_of f =
  [(float_of_bool f.constant);
   f.size;
   (float_of_bool f.return);
   (float_of_bool f.return_int);
   f.extern;
   (float_of_bool f.gvar);
   (float_of_bool f.from_string);
   (float_of_bool f.inside_loop);
   (float_of_bool f.update_param_in_loop);
   (float_of_bool f.use_ret_in_loop);
   (float_of_bool f.update_param_itself);
   (float_of_bool f.out_of_fun);
   f.finite;
   f.points_to;
   f.cstring;]

let string_of_trset trset = 
  BatMap.foldi (fun k v s -> 
      s^k^"\t"^(string_of_raw_feature v)^"\n") trset ""

let add_constant exps feat = 
  let rec const_condition = function
    | Const _ 
    | SizeOf _ | SizeOfE _ -> true
    | UnOp (_, e, _) -> const_condition e
    | _ -> false
  in
  if exps = [] then { feat with constant = true }
  else 
    List.fold_left (fun feat exp ->
      if const_condition (CilHelper.remove_cast exp) then
        { feat with constant = true }
      else feat) feat exps

let add_size exps feat = 
  { feat with size = float_of_int (List.length exps) }

let add_return lvo feat = 
  match lvo with 
    Some _ -> { feat with return = true }
  | _ -> feat 

let add_return_int pid lvo cfg feat = 
  match lvo with 
    Some x -> 
    { feat with return_int = (Cil.isIntegralType (Cil.typeOf (Cil.Lval x))) }
  | _ -> feat 

let sem_fun = ItvSem.run AbsSem.Strong ItvSem.Spec.empty

let add_points_to global node exps feat =
  if exps = [] then feat 
  else
    let locset = 
      PowLoc.remove Loc.null (Access.Info.accessof (AccessSem.accessof global node sem_fun global.mem))
    in
    { feat with points_to = float_of_int (PowLoc.cardinal locset) /. (float_of_int (List.length exps))  }

let add_extern global node exps feat =
  if exps = [] then feat
  else
    let pid = Node.get_pid node in
    let has_extern e = 
      let locset = Access.Info.accessof (AccessSem.accessof_eval pid e global.mem) in
      let v = Mem.lookup locset global.mem in
      let locset = PowLoc.join (Val.pow_loc_of_val v) (Val.array_of_val v |> ArrayBlk.pow_loc_of_array)  in 
      let locset = PowLoc.join (Val.pow_loc_of_val (Mem.lookup locset global.mem))
                   locset in 
      PowLoc.exists Loc.is_ext_allocsite locset
    in
    let count = List.fold_left (fun count e ->
      if has_extern e then count +. 1.0
      else count) 0.0 exps in
    { feat with extern = if count /. (float_of_int (List.length exps)) > 0.0 then 1.0 else 0.0 }

let add_gvar global node feat =
  let locset = Access.Info.accessof (AccessSem.accessof global node sem_fun global.mem) in
  let v = Mem.lookup locset global.mem in
  let locset = PowLoc.join (Val.pow_loc_of_val v) (Val.array_of_val v |> ArrayBlk.pow_loc_of_array) 
        |> PowLoc.join locset in 
  let locset = PowLoc.remove Loc.null locset in
  if (PowLoc.exists Loc.is_gvar locset) || 
     (PowLoc.exists (fun x -> (String.sub (Loc.to_string x) 0 3) = "_G_") locset)
  then 
    { feat with gvar = true }
  else feat

let add_finite global node exps feat = 
  let pid = Node.get_pid node in
  let is_finite e = 
    let v = ItvSem.eval pid e global.mem in
    let itv = Val.itv_of_val v in
    let arr = Val.array_of_val v in
    (Itv.is_finite itv) || (Itv.is_finite (ArrayBlk.sizeof arr))
  in
  if ((List.length exps = 3) && is_finite (List.nth exps 2)) ||
     ((List.length exps = 2) && is_finite (List.nth exps 1)) ||
     ((List.length exps = 1) && is_finite (List.nth exps 0)) ||
     (List.length exps = 0) then 
      { feat with finite = 1.0 }
    else feat

let add_cstring global cond_node cfg exps feat =
  let pid = Node.get_pid cond_node in
  let is_cstring e = 
    let use = Access.Info.useof (AccessSem.accessof_eval pid e global.mem) in
    PowLoc.exists (fun loc ->
        let v = Mem.lookup (PowLoc.singleton loc) global.mem in
        let nullpos = ArrayBlk.nullof (Val.array_of_val v) in 
        let v = Mem.lookup (PowLoc.singleton loc) global.mem in
        let locset = PowLoc.join (Val.pow_loc_of_val v) (Val.array_of_val v |> ArrayBlk.pow_loc_of_array)  in 
        let locset = PowLoc.fold (fun x -> BatSet.add (Loc.to_string x)) locset BatSet.empty in
        let strlib = List.map (fun x -> "__extern__"^x) ["getenv"; "rindex"; "index"; "strdup"; "strrchr"; "strstr"; "basename" ; "strtok" ; "fgets"; "_IO_getc"] in
        (BatSet.exists (fun x -> List.mem x strlib) locset) || 
           (not (Itv.is_bot nullpos) && (Itv.is_finite nullpos))) use
  in
  if ((List.length exps = 2) && is_cstring (List.nth exps 1)) ||
     ((List.length exps = 1) && is_cstring (List.nth exps 0)) ||
     (List.length exps = 0) then 
      { feat with cstring = 1.0 }
    else feat

let add_inside_loop global node exps pid scc_list feat =
  if List.exists (fun x -> List.mem (InterCfg.Node.get_cfgnode node) x) scc_list then 
    { feat with inside_loop = true }
  else feat

let add_use_ret_in_loop global node exps pid scc_list feat =
  let def = PowLoc.filter (fun x -> not (Loc.is_ext_allocsite x)) 
    (Access.Info.defof (AccessSem.accessof global node sem_fun global.mem)) in
  let b = 
    List.exists (fun loop ->
      List.exists (fun x -> 
          let inter_node = InterCfg.Node.make pid x in
          let use = Access.Info.useof (AccessSem.accessof global inter_node sem_fun global.mem) in
          (inter_node <> node) && ((PowLoc.meet use def) <> PowLoc.bot)) loop) scc_list
  in
  { feat with use_ret_in_loop = b }

let add_update_param_in_loop global node exps pid scc_list feat =
  let b = 
    List.exists (fun loop ->
      let use = List.fold_left (fun use e -> Access.Info.useof (AccessSem.accessof_eval pid e global.mem)) PowLoc.bot exps in
      List.exists (fun x -> 
          let inter_node = InterCfg.Node.make pid x in
          let def = Access.Info.defof (AccessSem.accessof global inter_node sem_fun global.mem) in
          (inter_node <> node) && ((PowLoc.meet use def) <> PowLoc.bot)) loop) scc_list
  in
  { feat with update_param_in_loop = b }

let add_update_param_itself global node exps pid scc_list feat =
  let access = AccessSem.accessof global node sem_fun global.mem in
  let def = PowLoc.filter (fun x -> not (Loc.is_ext_allocsite x)) (Access.Info.defof access) in
  let use = PowLoc.filter (fun x -> not (Loc.is_ext_allocsite x)) (Access.Info.useof access) in
  let b = PowLoc.bot <> (PowLoc.meet def use) in
    { feat with update_param_itself = b }

let add_out_of_fun global node cfg feat =
  let def = Access.Info.defof (AccessSem.accessof global node sem_fun global.mem) in
  let use_of_ret = IntraCfg.fold_node (fun intra_node locset ->
      match IntraCfg.find_cmd intra_node cfg with
        IntraCfg.Cmd.Creturn (Some e, _) -> 
          let node = InterCfg.Node.make (IntraCfg.get_pid cfg) intra_node in
          let use = Access.Info.useof (AccessSem.accessof global node sem_fun global.mem) in
          PowLoc.join use locset
      | _ -> locset) cfg PowLoc.bot
  in
  if (PowLoc.meet def use_of_ret) <> PowLoc.bot then
    { feat with out_of_fun = true }
  else feat 

let add_from f feat =
  if (Str.string_match (Str.regexp ".*string.*") (CilHelper.s_location f.vdecl) 0) then
    { feat with from_string = true }
  else feat

let ignore_libs = ["fprintf"; "printf"; "close"; "exit"; "perror"; "fileno"; "unlink"; "free"; "sprintf";
                   "fflush"; "fclose"; "__builtin_va_end"; "vfprintf"; "__builtin_va_start"; 
                   "signal"; "__assert_fail"; "putchar"; ]

let lib_feature global cfg trset = 
  let scc_list = IntraCfg.get_scc_list cfg |> List.filter (fun x -> List.length x > 1) in
  IntraCfg.fold_node (fun intra_node trset ->
    match IntraCfg.find_cmd intra_node cfg with 
      IntraCfg.Cmd.Ccall (lvo, Cil.Lval (Cil.Var f, Cil.NoOffset), exps, loc)
      when Global.is_undef f.vname global && 
           not (List.mem f.vname ignore_libs) -> (* undefined library functions *)
        let libid = (CilHelper.s_location loc)^":"^f.vname in
        let pid = IntraCfg.get_pid cfg in
        let node = InterCfg.Node.make pid intra_node in
        let feat = empty_feature
          |> add_constant exps 
          |> add_return lvo
          |> add_return_int pid lvo cfg
          |> add_size exps
          |> add_extern global node exps
          |> add_gvar global node
          |> add_inside_loop global node exps pid scc_list
          |> add_update_param_in_loop global node exps pid scc_list
          |> add_use_ret_in_loop global node exps pid scc_list
          |> add_update_param_itself global node exps pid scc_list
          |> add_out_of_fun global node cfg
          |> add_finite global node exps
          |> add_from f
          |> add_points_to global node exps
          |> add_cstring global node cfg exps
        in
        BatMap.add libid feat trset
    | _ -> trset) cfg trset

let normalize trset = 
  let max_size = 
    BatMap.fold (fun v max -> if v.size > max then v.size else max) trset 0.0
  in
  let max_points_to = 
    BatMap.fold (fun v max -> if v.points_to > max then v.points_to else max) trset 0.0
  in
  BatMap.map (fun feat -> 
    { feat with 
      size = feat.size /. max_size;
      points_to = feat.points_to /. max_points_to;
      }) trset 

let extract_feature : Global.t -> data 
= fun global ->
  let trset = Cil.foldGlobals global.file (fun trset glob ->
    match glob with 
      Cil.GFun (fd, _) ->
        (try 
          let cfg = InterCfg.cfgof global.icfg fd.svar.vname in
          lib_feature global cfg trset
        with _ -> trset)
    | _ -> trset) BatMap.empty 
  in
  if !Options.opt_debug then  
    (prerr_endline "== features for library ==";
    BatMap.iter (fun k v -> prerr_endline (k^"\n"^(string_of_feature v))) trset);
  normalize trset
  
let print_feature : data -> unit
= fun data -> 
  string_of_trset data |> print_string

let get_harmless_libs : Global.t -> lib BatSet.t
= fun global -> 
  if !Options.opt_bugfinder < 1 then BatSet.empty
  else
    let data = extract_feature global in
    let sparrow_bin_path = Unix.getenv "SPARROW_BIN_PATH" in
    let sparrow_data_path = Unix.getenv "SPARROW_DATA_PATH" in
    let py = Lymp.init ~exec:"python2" sparrow_bin_path in
    let py_module = Lymp.get_module py "harmless_unsoundness" in
    let classifier = Lymp.Pyref (Lymp.get_ref py_module "load" [Lymp.Pystr (sparrow_data_path ^ "/harmless_lib_clf")]) in
    let set = BatMap.foldi (fun l fvec loops -> 
        let vec = feature_vector_of fvec in
        let vec = Lymp.Pylist (List.map (fun x -> Lymp.Pyfloat x) vec) in 
        let b = Lymp.get_bool py_module "is_harmless" [classifier; vec] in
        if b then BatSet.add l loops else loops
        ) data BatSet.empty
    in
    Lymp.close py;
    set

let collect : Global.t -> lib BatSet.t
= fun global ->
  BatSet.union (get_harmless_libs global) !Options.opt_unsound_lib
