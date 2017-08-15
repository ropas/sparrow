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
(** Vocabularies *)

let (<<<) f g = fun x -> f (g x)
let (>>>) f g = fun x -> g (f x)
let ($>) x f = match x with Some s -> f s | None -> None
let (&>) x f = match x with Some s -> Some (f s) | None -> None
let (@) l1 l2 = BatList.append l1 l2
let id x = x
let flip f = fun y x -> f x y
let cond c f g x = if c then f x else g x
let opt c f x = if c then f x else x
let tuple x = (x, x)

let domof m = BatMap.foldi (fun k _ set -> BatSet.add k set) m BatSet.empty

(** This applies [List.fold_left], but the argument type is the same with
    [PSet.fold].  *)
let list_fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
= fun f list init ->
  List.fold_left (flip f) init list

let list_fold2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
= fun f list1 list2 init ->
  let f' acc a b = f a b acc in
  List.fold_left2 f' init list1 list2

let list_rev : 'a list -> 'a list
= fun l ->
  let rec list_rev_rec l1 l2 =
    match l1 with
    | [] -> l2
    | a :: b -> list_rev_rec b (a :: l2) in
  list_rev_rec l []

let append_opt : 'a option -> 'a list -> 'a list
= fun x l ->
  match x with None -> l | Some x -> x::l
let find_opt : 'a -> ('a, 'b) BatMap.t -> 'b option
= fun k m ->
  try Some (BatMap.find k m) with
  | Not_found -> None

let find_def : 'a -> ('a, 'b) BatMap.t -> 'b -> 'b
= fun k m default ->
  try BatMap.find k m with _ -> default

let link_by_sep sep s acc = if acc = "" then s else acc ^ sep ^ s

let string_of_list ?(first="[") ?(last="]") ?(sep=";") : ('a -> string)
  -> ('a list) -> string
= fun string_of_v list ->
  let add_string_of_v v acc = link_by_sep sep (string_of_v v) acc in
  first ^ list_fold add_string_of_v list "" ^ last

let string_of_set ?(first="{") ?(last="}") ?(sep=",") : ('a -> string)
  -> ('a BatSet.t) -> string
= fun string_of_v set ->
  let add_string_of_v v acc = link_by_sep sep (string_of_v v) acc in
  first ^ BatSet.fold add_string_of_v set "" ^ last

let string_of_map ?(first="{") ?(last="}") ?(sep=",\n") ?(indent="") : ('a -> string)
  -> ('b -> string) -> (('a, 'b) BatMap.t) -> string
= fun string_of_k string_of_v map ->
  let add_string_of_k_v k v acc =
    let str = string_of_k k ^ " -> " ^ string_of_v v in
    link_by_sep (sep^indent) str acc in
  if BatMap.is_empty map then "empty"
  else indent ^ first ^ BatMap.foldi add_string_of_k_v map "" ^ last

let i2s = string_of_int

let list2set l = list_fold BatSet.add l BatSet.empty
let set2list s = BatSet.fold (fun x l -> x::l) s []

let set_union_small_big small big = BatSet.fold BatSet.add small big

(* print progress bar *)
let prerr_progressbar ?(itv=1) : int -> int -> unit
= fun n total ->
  if !Options.verbose < 1 || !Options.nobar then () else
  if n mod itv = 0 || n = total then
    let v = n * 60 / total in
    let u = 60-v in
    let rec ps f s n = if n=0 then () else (f s;ps f s (n-1)) in
      prerr_string("\r[");
      ps prerr_string "=" v;
      ps prerr_string "." u;
      prerr_string("] ("^(string_of_int n)^"/"^(string_of_int total)^")     ");
      if n = total then prerr_newline ();
      flush stderr
  else ()

(* fixpoint operator for set *)
let rec fix : ('a BatSet.t -> 'a BatSet.t) -> 'a BatSet.t -> 'a BatSet.t 
= fun f init ->
  let next = f init in
    if BatSet.subset next init then init
    else fix f next

let my_prerr_endline str = if !Options.verbose >= 1 then prerr_endline str
let my_prerr_newline () = if !Options.verbose >= 1 then prerr_newline ()
let my_prerr_string str = if !Options.verbose >= 1 then prerr_string str

let prerr_memory_usage () =
  if !Options.verbose <= 1 then ()
  else
    let stat = Gc.stat () in
    let kB_of_word w = w * Sys.word_size / 1000 / 8 in
    let live_kB = kB_of_word stat.Gc.live_words in
    prerr_endline ("live memory : " ^ string_of_int live_kB ^ " kB")
