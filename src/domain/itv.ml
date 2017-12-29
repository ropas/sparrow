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
open Vocab

(* ****************** *
 * Widening threshold *
 * ****************** *)

let threshold = BatSet.of_list [0;1;16;64]


(* ************************ *
 * Integer = Z + {-oo, +oo} *
 * ************************ *)

module Integer =
struct
  type t = Int of int | MInf | PInf [@@deriving compare]

  let pinf = PInf
  let minf = MInf
  let zero = Int 0
  let of_int i = Int i

  let to_string : t -> string = function
    | Int i -> string_of_int i
    | PInf -> "+oo"
    | MInf -> "-oo"

  let pp fmt x = Format.fprintf fmt "%s" (to_string x)

  let le : t -> t -> bool = fun x y ->
    match x, y with
    | MInf, _ -> true
    | _, PInf -> true
    | Int i, Int j -> i <= j
    | _, _ -> false

  let eq : t -> t -> bool = fun x y ->
    match x, y with
    | MInf, MInf
    | PInf, PInf -> true
    | Int i, Int j -> i = j
    | _, _ -> false

  let absolute = function
    | Int i -> Int (abs i)
    | MInf -> PInf
    | PInf -> PInf

  let min : t -> t -> t = fun x y -> if le x y then x else y
  let max : t -> t -> t = fun x y -> if le x y then y else x

  let lower_widen : t -> t -> t = fun x y ->
    if le y x then
      if eq y x then y else
        let filtered = BatSet.filter (fun k -> le (Int k) y) threshold in
        if BatSet.is_empty filtered
        then MInf else Int (BatSet.max_elt filtered)
    else x

  let upper_widen : t -> t -> t = fun x y ->
    if le x y then
      if eq x y then y else
        let filtered = BatSet.filter (fun k -> le y (Int k)) threshold in
        if BatSet.is_empty filtered
      then PInf else Int (BatSet.min_elt filtered)
    else x

  let lower_narrow : t -> t -> t = fun x y ->
    if le x y then
      if eq x MInf || BatSet.exists (fun k -> x = Int k) threshold then y
      else x
    else invalid_arg ("itv.ml: Integer.lower_narrow (x, y). y < x : "^(to_string y)^" < "^(to_string x))

  let upper_narrow : t -> t -> t = fun x y ->
    if le y x then
      if eq x PInf || BatSet.exists (fun k -> x = Int k) threshold then y
      else x
    else invalid_arg "itv.ml: Integer.upper_narrow (x, y). x < y"

  let plus x y =
    match x, y with
    | Int n1, Int n2 -> Int (n1 + n2)
    | PInf, MInf | MInf, PInf -> invalid_arg "itv.ml: Integer.plus"
    | PInf, _ -> PInf
    | MInf, _ -> MInf
    | _, PInf -> PInf
    | _, MInf -> MInf

  let minus x y =
    match x, y with
    | Int n1, Int n2 -> Int (n1 - n2)
    | PInf, PInf | MInf, MInf -> invalid_arg "itv.ml: Integer.minus"
    | PInf, _ -> PInf
    | MInf, _ -> MInf
    | _, PInf -> MInf
    | _, MInf -> PInf

  let times x y =
    match x, y with
    | Int n1, Int n2 -> Int (n1 * n2)
    | PInf, PInf
    | MInf, MInf -> PInf
    | PInf, MInf
    | MInf, PInf -> MInf
    | PInf, Int n
    | Int n, PInf ->
      if n < 0 then MInf else
      if n > 0 then PInf else
        Int 0
    | (MInf, Int n)
    | (Int n, MInf) ->
      if n < 0 then PInf else
      if n > 0 then MInf else
        Int 0

  let divide x y =
    match x, y with
    | Int n1, Int n2 ->
      if n2 = 0 then invalid_arg "itv.ml: Integer.divide (_, 0)" else Int (n1 / n2)
    | PInf, PInf
    | MInf, MInf -> PInf
    | MInf, PInf
    | PInf, MInf -> MInf
    | MInf, Int n ->
      if n < 0 then PInf else
      if n > 0 then MInf else
        invalid_arg "itv.ml: Integer.divide (-oo, 0)"
    | PInf, Int n ->
      if n < 0 then MInf else
      if n > 0 then PInf else
        invalid_arg "itv.ml: Integer.divide (+oo, 0)"
    | Int _,  PInf
    | Int _,  MInf -> Int 0

  let min4 : t -> t -> t -> t -> t = fun x y z w ->
    min (min x y) (min z w)

  let max4 : t -> t -> t -> t -> t = fun x y z w ->
    max (max x y) (max z w)
end

(** {6 Main definitions of interval} *)
open Integer

type t = V of Integer.t * Integer.t | Bot [@@deriving compare]

let zero = V (Int 0, Int 0)
let one = V (Int 1, Int 1)
let pos = V (Int 1, PInf)
let neg = V (MInf, Int (-1))
let nat = V (Int 0, PInf)

let upper = function V (_, Int x) -> x | _ -> invalid_arg "Itv.upper"
let lower = function V (Int x, _) -> x | _ -> invalid_arg "Itv.lower"
let upper_integer = function V (_, x) -> x | _ -> invalid_arg "Itv.upper_integer"
let lower_integer = function V (x, _) -> x | _ -> invalid_arg "Itv.lower_integer"

let of_int : int -> t = fun i -> V (Int i, Int i)
let of_ints : int -> int -> t = fun lb ub -> V (Int lb, Int ub)
let of_integer : Integer.t -> Integer.t -> t = fun l u -> V (l, u)

let to_string : t -> string = function
  | Bot -> "bot"
  | V (l, u) -> "["^(Integer.to_string l)^", "^(Integer.to_string u)^"]"

let pp fmt = function
  | Bot -> Format.fprintf fmt "bot"
  | V (l, u) -> Format.fprintf fmt "[%a, %a]" Integer.pp l Integer.pp u

let to_json : t -> Yojson.Safe.json = fun itv ->
  `String (to_string itv)

let is_bot : t -> bool = function
  | Bot -> true
  | V (l, u) -> l = PInf || u = MInf || not (Integer.le l u)


(** Normalizes invalid intervals such as [\[u, l\]] with [u > l] to
    [Bot].*)
let normalize x = if is_bot x then Bot else x

let absolute = function
  | Bot -> Bot
  | V (l, u) ->
    if Integer.le Integer.zero l then V (l, u)
    else if Integer.le l Integer.zero && Integer.le Integer.zero u then V (Integer.zero, Integer.max (Integer.absolute l) u)
    else V (Integer.absolute u, Integer.absolute l)

let le : t -> t -> bool = fun x y ->
  if is_bot x then true else
  if is_bot y then false else
    match x, y with
    | V (l1, u1), V (l2, u2) -> Integer.le l2 l1 && Integer.le u1 u2
    | _, _ -> assert false


let eq : t -> t -> bool = fun x y ->
  if is_bot x && is_bot y then true else
  if is_bot x || is_bot y then false else
    match x, y with
    | V (l1, u1), V (l2, u2) -> Integer.eq l2 l1 && Integer.eq u1 u2
    | _, _ -> assert false


let top : t = V (MInf, PInf)
let bot : t = Bot


let join : t -> t -> t = fun x y ->
  if le x y then y else
  if le y x then x else
  if is_bot x then normalize y else
  if is_bot y then normalize x else
    match x, y with
    | V (l1, u1), V (l2, u2) -> V (Integer.min l1 l2, Integer.max u1 u2)
    | _, _ -> assert false


let meet : t -> t -> t = fun x y ->
  if le x y then x else
  if le y x then y else
  if is_bot x then Bot else
  if is_bot y then Bot else
    match x, y with
    | V (l1, u1), V (l2, u2) ->
      normalize (V (Integer.max l1 l2, Integer.min u1 u2))
    | _, _ -> assert false


let widen : t -> t -> t = fun x y ->
  if x == y then x else
  if is_bot x then normalize y else
  if is_bot y then normalize x else
    match x, y with
    | V (l1, u1), V (l2, u2) ->
      V (Integer.lower_widen l1 l2, Integer.upper_widen u1 u2)
    | _, _ -> assert false


let narrow : t -> t -> t = fun x y ->
  if x == y then x else
  if is_bot y then Bot else
  if is_bot x then invalid_arg "itv.ml: narrow(bot, _)" else
    match x, y with
    | V (l1, u1), V (l2, u2) ->
      V (Integer.lower_narrow l1 l2, Integer.upper_narrow u1 u2)
    | _, _ -> assert false


(** {6 Auxiliary functions for interval} *)

let open_right (x:t) : bool =
  if is_bot x then false else
    match x with
    | V (_, PInf) -> true
    | _ -> false

let close_left (x:t) : bool =
  if is_bot x then false else
    match x with
    | V (Int _, _) -> true
    | _ -> false


let open_left (x:t) : bool =
  if is_bot x then false else
    match x with
    | V (MInf, _) -> true
    | _ -> false


let is_range (x:t) : bool = not (is_bot x)

let is_const (x:t) : bool =
  match x with
  | V (Int x, Int y) -> x = y
  | _ -> false

let is_finite (x:t) : bool =
  if is_bot x then false else
    match x with
    | V (Int _, Int _) -> true
    | _ -> false

let is_negative (x:t) : bool =
  if is_bot x then false else
    match x with
    | V (MInf,  _) -> true
    | V (Int x, Int _) -> x < 0
    | _ -> false


let height (x:t) : int =
  let h_bound = 1000 in
  if is_bot x then 0 else
    match x with
    | V (Int l, Int u) -> if u - l + 1 > h_bound then h_bound else u - l + 1
    | _ -> h_bound


let diff (x:t) : int =
  if is_bot x then 0 else
    match x with
    | V (Int l, Int u) -> u - l
    | _ -> 0


(** {6 Binary/Unary operations for interval} *)

let plus (x:t) (y:t) : t =
  if is_bot x || is_bot y then Bot else
    match x, y with
    | V (l1, u1), V (l2, u2) -> V (Integer.plus l1 l2, Integer.plus u1 u2)
    | _, _ -> assert false

let minus (x:t) (y:t) : t =
  if is_bot x || is_bot y then Bot else
    match x, y with
    | V (l1, u1), V (l2, u2) -> V (Integer.minus l1 u2, Integer.minus u1 l2)
    | _, _ -> assert false

let times (x:t) (y:t) : t =
  if is_bot x || is_bot y then Bot else
    match x, y with
    | V (l1, u1), V (l2, u2) ->
      let x1 = Integer.times l1 l2 in
      let x2 = Integer.times l1 u2 in
      let x3 = Integer.times u1 l2 in
      let x4 = Integer.times u1 u2 in
      V (Integer.min4 x1 x2 x3 x4, Integer.max4 x1 x2 x3 x4)
    | _, _ -> assert false

let divide (x:t) (y:t) : t =
  if is_bot x || is_bot y then Bot else
  if le (V (Int 0, Int 0)) y then top else
    match x, y with
    | V (l1, u1), V (l2, u2) ->
      let x1 = Integer.divide l1 l2 in
      let x2 = Integer.divide l1 u2 in
      let x3 = Integer.divide u1 l2 in
      let x4 = Integer.divide u1 u2 in
      V (Integer.min4 x1 x2 x3 x4, Integer.max4 x1 x2 x3 x4)
    | _, _ -> assert false

let false_itv : t = V (Int 0, Int 0)
let true_itv : t = V (Int 1, Int 1)
let unknown_bool_itv : t = V (Int 0, Int 1)

let l_and (x:t) (y:t) : t =
  if is_bot x || is_bot y then
    Bot
  else if eq false_itv x || eq false_itv y then
    false_itv
  else if not (le false_itv x) && not (le false_itv y) then
    true_itv
  else
    unknown_bool_itv


let l_or (x:t) (y:t) : t =
  if is_bot x || is_bot y then
    Bot
  else if eq false_itv x && eq false_itv y then
    false_itv
  else if not (le false_itv x) || not (le false_itv y) then
    true_itv
  else
    unknown_bool_itv


let eq_itv (x:t) (y:t) : t =
  if is_bot x || is_bot y then Bot else
    match x, y with
    | V (Int l1, Int u1), V (Int l2, Int u2)
      when l1 = u1 && u1 = l2 && l2 = u2 -> true_itv
    | V (_, Int u1), V (Int l2, _) when u1 < l2 -> false_itv
    | V (Int l1, _), V (_, Int u2) when u2 < l1 -> false_itv
    | _, _ -> unknown_bool_itv


let ne_itv (x:t) (y:t) : t =
  if is_bot x || is_bot y then Bot else
    match x, y with
    | V (Int l1, Int u1), V (Int l2, Int u2)
      when l1 = u1 && u1 = l2 && l2 = u2 -> false_itv
    | V (_, Int u1), V (Int l2, _) when u1 < l2 -> true_itv
    | V (Int l1, _), V (_, Int u2) when u2 < l1 -> true_itv
    | _, _ -> unknown_bool_itv


let lt_itv (x:t) (y:t) : t =
  if is_bot x || is_bot y then Bot else
    match x, y with
    | V (_, Int u1), V (Int l2, _) when u1 < l2 -> true_itv
    | V (Int l1, _), V (_, Int u2) when u2 <= l1 -> false_itv
    | _, _ -> unknown_bool_itv


let le_itv (x:t) (y:t) : t =
  if is_bot x || is_bot y then Bot else
    match x, y with
    | V (_, Int u1), V (Int l2, _) when u1 <= l2 -> true_itv
    | V (Int l1, _), V (_, Int u2) when u2 < l1 -> false_itv
    | _, _ -> unknown_bool_itv

let gt_itv (x:t) (y:t) : t = lt_itv y x
let ge_itv (x:t) (y:t) : t = le_itv y x

let l_not (x:t) : t =
  if is_bot x then Bot else
  if eq false_itv x then true_itv else
  if le false_itv x then unknown_bool_itv else
    false_itv

let unknown_binary (x:t) (y:t) : t =
  if is_bot x || is_bot y then bot
  else top

let unknown_unary (x:t) : t =
  if is_bot x then Bot
  else top

let l_shift (x:t) (y:t) : t =
  match x, y with
    V (Int l1, Int u1), V (Int l2, Int u2) when l1 = u1 && l2 = u2 ->
      let x = l1 lsl l2 in
      V (Int x, Int x)
  | _ -> unknown_binary x y

let itv_of_type : Cil.typ -> t = function
  | Cil.TInt (Cil.IUChar, _) -> of_ints 0 255
  | Cil.TInt (Cil.IUShort, _) -> of_ints 0 65535
  | Cil.TInt (Cil.IUInt, _) | Cil.TInt (Cil.ILong, _)
  | Cil.TInt (Cil.IULongLong, _) -> of_ints 0 4294967295
  | Cil.TInt (Cil.IChar, _) -> of_ints (-128) 255
  | Cil.TInt (Cil.IShort, _) -> of_ints (-32768) 32767
  | Cil.TInt (Cil.IInt, _) | Cil.TInt (Cil.IULong, _)
  | Cil.TInt (Cil.ILongLong, _) -> of_ints (-2147483648) 2147483648
  | _ -> top

let cast : Cil.typ -> Cil.typ -> t -> t
= fun from_typ to_typ itv ->
  if !Options.int_overflow then
  begin
  match itv with
    Bot -> Bot
  | _ ->
    let (from_size, to_size) =
      ((try CilHelper.byteSizeOf from_typ |> of_int with _ -> top),
       (try CilHelper.byteSizeOf to_typ |> of_int with _ -> top))
    in
    if CilHelper.is_unsigned from_typ && CilHelper.is_unsigned to_typ then
      if from_size <= to_size then itv
      else if Integer.le (upper_integer itv) (upper_integer (itv_of_type to_typ)) then itv
      else top (* possibly overflow *)
    else if not (CilHelper.is_unsigned from_typ) && CilHelper.is_unsigned to_typ then
      if from_size <= to_size then absolute itv
      else if Integer.le (upper_integer itv) (upper_integer (itv_of_type to_typ)) then itv
      else top (* possibly overflow *)
    else if CilHelper.is_unsigned from_typ && not (CilHelper.is_unsigned to_typ) then
      if from_size < to_size then itv
      else if Integer.le (upper_integer itv) (upper_integer (itv_of_type to_typ)) then itv
      else top (* possibly overflow *)
    else
      if from_size <= to_size then itv
      else if Integer.le (upper_integer itv) (upper_integer (itv_of_type to_typ)) then itv
      else top (* possibly overflow *)
  end
  else
  begin
    if CilHelper.is_unsigned to_typ then absolute itv
    else itv
  end

let prune : Cil.binop -> t -> t -> t = fun op x y ->
  if is_bot x || is_bot y then Bot else
    let pruned =
      match op, x, y with
      | Cil.Lt, V (a, b), V (c, d) -> V (a, Integer.min b (Integer.minus d (Int 1)))
      | Cil.Gt, V (a, b), V (c, d) -> V (Integer.max a (Integer.plus c (Int 1)), b)
      | Cil.Le, V (a, b), V (c, d) -> V (a, Integer.min b d)
      | Cil.Ge, V (a, b), V (c, d) -> V (Integer.max a c, b)
      | Cil.Eq, V (a, b), V (c, d) -> meet x y
      | Cil.Ne, V (a, b), V (c, d) when Integer.eq b c && Integer.eq c d ->
        V (a, Integer.minus b (Int 1))
      | Cil.Ne, V (a, b), V (c, d) when Integer.eq a c && Integer.eq c d ->
        V (Integer.plus a (Int 1), b)
      | Cil.Ne, V _, V _ -> x
      | _ -> invalid_arg "itv.ml:prune" in
    normalize pruned
