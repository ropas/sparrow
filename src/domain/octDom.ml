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
(** Octagon domain *)
open Cil
open BasicDom
open Global
open Vocab
open Apron

module OctLoc = 
struct 
  type t = Loc of Loc.t | Size of Allocsite.t [@@deriving compare]
  let dummy = Loc Loc.dummy
  let of_loc l = Loc l
  let of_size a = Size a
  let to_string = function Loc l -> Loc.to_string l | Size l -> Allocsite.to_string l ^ ":size"
  let to_var : t -> Apron.Var.t
    = fun l -> to_string l |> Apron.Var.of_string
  let pp fmt = function
    | Loc l -> Format.fprintf fmt "%a" Loc.pp l
    | Size a -> Format.fprintf fmt "%a" Allocsite.pp a
end
module PowOctLoc = 
struct 
  include PowDom.MakeCPO(OctLoc)
  let empty = bot
  let of_locs : PowLoc.t -> t
  = fun locs -> PowLoc.fold (fun x -> add (OctLoc.of_loc x)) locs empty
  let of_sizes : Allocsite.t BatSet.t -> t
  = fun locs -> BatSet.fold (fun x -> add (OctLoc.of_size x)) locs empty
  let to_string x = 
    if is_empty x then "{}"
    else to_string x
end

module Pack = PowOctLoc

module PackConf = 
struct
  include PowDom.MakeCPO(Pack)
  
  (* cache *)
  let loc2pack : (OctLoc.t, Pack.t) Hashtbl.t = Hashtbl.create 1000

  let get_pack : t -> OctLoc.t -> Pack.t 
  = fun packconf lv ->
    try Hashtbl.find loc2pack lv 
    with Not_found ->
      begin
      try
        let pack = choose (filter (Pack.mem lv) packconf) in
        Hashtbl.add loc2pack lv pack;
        pack
      with _ -> 
        (if !Options.debug then prerr_endline ("get_pack.Not_found : " ^ (OctLoc.to_string lv)));
        Pack.singleton lv
      end

  let get_all_octlocs : ItvDom.Table.t -> Pack.t 
  = fun itvinputof ->
    ItvAnalysis.Table.fold (fun _ mem pack ->
      ItvDom.Mem.foldi (fun x v pack -> 
        if (ItvDom.Val.pow_proc_of_val v <> PowProc.bot)
        then pack
        else 
          let plocs = ArrayBlk.allocsites_of_array (ItvDom.Val.array_of_val v) in
          pack
          |> opt (ItvDom.Val.itv_of_val v <> Itv.bot) (Pack.add (OctLoc.of_loc x))
          |> BatSet.fold (fun x -> Pack.add (OctLoc.of_size x)) plocs) mem pack
    ) itvinputof Pack.bot
   
  let fullpack itvinputof = get_all_octlocs itvinputof |> singleton

  let make : ItvDom.Table.t -> t -> t
  = fun itvinputof packs ->
    let all_octlocs = get_all_octlocs itvinputof in
    let relational = fold PowOctLoc.union packs PowOctLoc.empty in
    let non_relational = PowOctLoc.diff all_octlocs relational in
    PowOctLoc.fold (fun x -> add (PowOctLoc.singleton x)) non_relational packs

  let print packconf = 
    BatSet.iter (fun s ->
        prerr_string "{";
        Pack.iter (fun x -> prerr_string ((OctLoc.to_string x)^", ")) s;
        prerr_string "}, ") packconf

  let print_info packconf = 
    let total = cardinal packconf in
    let (non_singleton, rel_var) = 
      fold (fun x (pack, var) -> 
        if Pack.cardinal x > 1 then (pack + 1, var + (Pack.cardinal x))
        else (pack, var)) packconf (0,0) 
    in
    prerr_endline "=== Packing Configuration ===";
    prerr_endline ("=== # total packs         : "^(string_of_int total));
    prerr_endline ("=== # non-singleton packs : "^(string_of_int non_singleton));
    prerr_endline ("=== # relational variables : "^(string_of_int rel_var));
    iter (fun x ->
        if Pack.cardinal x > 1 then
        begin
          prerr_int (Pack.cardinal x);
          prerr_string " : {";
          Pack.iter (fun x -> prerr_string ((OctLoc.to_string x)^", ")) x;
          prerr_endline "}, "
        end
      ) packconf
end

type packconf = PackConf.t

module Octagon = 
struct 
  type t = V of oct | Bot 
  and oct = Oct.t Abstract1.t

  let man = Oct.manager_alloc ()
  let compare = compare

  let bot = Bot
  let top pack =
    let l = Pack.fold (fun x l -> (OctLoc.to_string x)::l) pack [] in
    let env = Environment.make (Array.of_list (List.map Apron.Var.of_string l)) [||] in
    V (Abstract1.top man env)

  let is_bot x = function Bot -> true | _ -> false

  let le : t -> t -> bool 
  = fun x y ->
    match (x, y) with 
    | (Bot, _) -> true
    | (V x, V y) -> Abstract1.is_leq man x y
    | (_, _) -> false

  let eq : t -> t -> bool 
  = fun x y -> 
    match (x, y) with
    | (Bot, Bot) -> true
    | (V x, V y) -> Abstract1.is_eq man x y
    | (_, _) -> false
  
  let is_bot = function Bot -> true | V o -> Abstract1.is_bottom man o 

  let interval_to_string itv = 
    let lb = if Scalar.is_infty itv.Interval.inf = -1 then "-oo" else Scalar.to_string itv.Interval.inf in
    let ub = if Scalar.is_infty itv.Interval.sup = 1 then "+oo" else Scalar.to_string itv.Interval.sup in
    "["^lb^", "^ub^"]"

  let to_string = function
    | Bot -> "Oct.Bot"
    | V o ->
      let env = Abstract1.env o in
      let (vars, _) = Environment.vars env in
      Array.fold_left (fun s x ->
          Array.fold_left (fun s y ->
              if x = y then 
                let itv = Abstract1.bound_variable man o x in
                (if Interval.is_top itv then s
                 else s^(Var.to_string x)^" = "^(interval_to_string itv)^"\n")
              else
                let sub = Abstract1.bound_texpr man o (Texpr1.of_expr env (Texpr1.Binop (Texpr1.Sub, Texpr1.Var x, Texpr1.Var y, Texpr1.Int, Texpr1.Near))) in
                s
                ^(if Interval.is_top sub then "" else ((Var.to_string x)^" - "^(Var.to_string y)^" = "^(interval_to_string sub)^"\n"))
            ) s vars) "\n" vars

  let pp fmt x = Format.fprintf fmt "%s" (to_string x)

  let bound_variable o v = 
    match o with 
      Bot -> Apron.Interval.bottom
    | V o -> Abstract1.bound_variable man o v

  let interval_to_itv i =
    let f scalar = 
      match Scalar.is_infty scalar with 
        -1 -> Itv.Integer.minf
      | 1 -> Itv.Integer.pinf
      | _ -> 
      begin
        match scalar with
          Scalar.Float f -> Itv.Integer.of_int (int_of_float f)
        | Scalar.Mpqf f -> Itv.Integer.of_int (int_of_float (Mpqf.to_float f))
        | Scalar.Mpfrf f -> Itv.Integer.of_int (int_of_float (Mpfrf.to_float f))
      end
    in
    let (lb, ub) = (f i.Interval.inf, f i.Interval.sup) in
    if not (Itv.Integer.le lb ub) then Itv.bot 
    else Itv.of_integer lb ub

  let itv_of_var : OctLoc.t -> t -> Itv.t
  = fun x o ->
    match o with 
    | V o -> 
      Abstract1.bound_variable man o (Apron.Var.of_string (OctLoc.to_string x))
      |> interval_to_itv
    | _ -> Itv.bot

  let bound_texpr o e = 
    match o with 
      Bot -> Apron.Interval.bottom
    | V o -> Abstract1.bound_texpr man o (Texpr1.of_expr (Abstract1.env o) e)

  let itv_of_expr : Texpr1.expr -> t -> Itv.t
  = fun exp oct -> 
    interval_to_itv (bound_texpr oct exp)

  let to_json x = Yojson.Safe.(`String "") (* TODO *)

  let set : OctLoc.t -> Texpr1.expr -> t -> t
  = fun lv texpr oct ->
    match oct with
      V o ->
        let var = OctLoc.to_var lv in 
        let env = Abstract1.env o in
        let texpr = Texpr1.of_expr env texpr in
        let o = Abstract1.assign_texpr man o var texpr None in
        if is_bot oct then bot
        else V o
    | Bot -> bot

  let forget : OctLoc.t -> t -> t
  = fun lv oct ->
    match oct with 
      V o -> V (Abstract1.forget_array man o [| OctLoc.to_var lv |] false)
    | Bot -> bot 

  let prune : OctLoc.t -> Texpr1.expr -> Tcons1.typ -> t -> t
  = fun lv texpr typ oct ->
    match oct with 
      V o ->
        let env = Abstract1.env o in
        let tcons = Tcons1.make (Texpr1.of_expr env texpr) typ in
        let earray = Tcons1.array_make env 1 in
        Tcons1.array_set earray 0 tcons;
        V (Abstract1.meet_tcons_array man o earray)
    | _ -> oct
    
  let join : t -> t -> t 
  = fun x y -> 
    match (x, y) with
    | x, Bot
    | Bot, x -> x
    | V x, V y -> 
      V (Abstract1.join man x y)
      
  let meet : t -> t -> t
  = fun x y ->
    match (x, y) with
    | _, Bot
    | Bot, _ -> Bot
    | V x, V y ->
      V (Abstract1.meet man x y)

  let widen : t -> t -> t 
  = fun x y ->
    match (x, y) with
    | x, Bot
    | Bot, x -> x
    | V x, V y -> 
      V (Abstract1.widening man x y)

  let narrow : t -> t -> t
  = fun x y -> y (* TODO *)
end


module Mem = 
struct 
  include InstrumentedMem.Make (MapDom.MakeCPO (Pack) (Octagon))

  let top packconf = 
    PackConf.fold (fun pack -> add pack (Octagon.top pack)) packconf bot
  let init = top

  let to_string : t -> string = fun x ->
    if is_empty x then "Bot" else
      foldi (fun pack v s ->
        if Pack.cardinal pack > 2 then 
          s^(Pack.to_string pack)^" -> "^(Octagon.to_string v)
        else s) x ""
end
