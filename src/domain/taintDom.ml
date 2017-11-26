open BasicDom
open InterCfg

module IntOverflow =
struct
  type t = Bot | Top
  let to_string = function
    | Bot -> "No Overflow"
    | Top -> "May Overflow"

  let compare = compare
  let bot = Bot
  let top = Top
  let le x y =
    match x, y with
    | Bot, _ -> true
    | Top, Bot -> false
    | Top, Top -> true

  let eq x y =
    match x, y with
    | Bot, Bot -> true
    | Top, Top -> true
    | _ -> false

  let join x y =
    match x, y with
    | Bot, Bot -> Bot
    | Top, _ | _, Top -> Top

  let meet x y =
    match x, y with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | _ -> Top

  let is_bot x = x = Bot

  let widen = join
  let narrow = meet
  let pp fmt x = Format.fprintf fmt "%s" (to_string x)
end

module UserInput =
struct
  module Source =
  struct
    type t = Node.t * Cil.location
    let to_string (node, loc) =
      Node.to_string node ^ "@" ^ CilHelper.s_location loc

    let compare x y = Node.compare (fst x) (fst y)
    let pp fmt x = Format.fprintf fmt "%s" (to_string x)
  end
  include PowDom.MakeLAT(Source)
  let make node loc = singleton (node, loc)
  let is_bot = is_empty
  let is_taint x = not (is_bot x)
end

module Val =
struct
  type t = {
    int_overflow : IntOverflow.t;
    user_input : UserInput.t;
  }

  let int_overflow x = x.int_overflow
  let user_input x = x.user_input

  let to_string t =
    "{ int_overflow: " ^ IntOverflow.to_string t.int_overflow
    ^ ", user_input: " ^ UserInput.to_string t.user_input ^ " }"

  let compare = compare
  let bot = { int_overflow = IntOverflow.bot; user_input = UserInput.bot }
  let top = { int_overflow = IntOverflow.top; user_input = UserInput.top }
  let input_value node loc = { top with user_input = UserInput.make node loc }
  let le x y =
    (IntOverflow.le x.int_overflow y.int_overflow)
    && (UserInput.le x.user_input y.user_input)

  let eq x y =
    (IntOverflow.eq x.int_overflow y.int_overflow)
    && (UserInput.eq x.user_input y.user_input)

  let join x y =
    { int_overflow = IntOverflow.join x.int_overflow y.int_overflow;
      user_input = UserInput.join x.user_input y.user_input }

  let meet x y =
    { int_overflow = IntOverflow.meet x.int_overflow y.int_overflow;
      user_input = UserInput.meet x.user_input y.user_input }

  let is_bot x = x = bot

  let widen = join
  let narrow = meet
  let pp fmt x =
    Format.fprintf fmt "{ int_overflow: %a, user_input: %a }"
      IntOverflow.pp x.int_overflow UserInput.pp x.user_input
end

module Mem =
struct
  include InstrumentedMem.Make(MapDom.MakeCPO (Loc) (Val))

  let lookup : PowLoc.t -> t -> Val.t = fun locs mem ->
    if eq mem bot then Val.bot
    else
      let find_join loc acc = Val.join acc (find loc mem) in
      PowLoc.fold find_join locs Val.bot

  let strong_update : PowLoc.t -> Val.t -> t -> t
  = fun locs v mem ->
    PowLoc.fold (fun x -> add x v) locs mem

  let weak_update : PowLoc.t -> Val.t -> t -> t
  = fun locs v mem ->
    PowLoc.fold (fun x -> weak_add x v) locs mem
end

module Table = MapDom.MakeCPO (Node) (Mem)
