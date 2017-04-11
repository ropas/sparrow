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
module type S = 
sig
  include MapDom.CPO
  module Access : Access.S with type Loc.t = A.t and type PowLoc.t = PowA.t
  val init_access : unit -> unit
  val return_access : unit -> Access.t
end 

module Make (Mem : MapDom.CPO) = 
struct
  include Mem

  module Loc = A
  module Val = B
  module Access = Access.Make(Mem)
  let access = ref Access.empty
  let access_mode = ref false
  let init_access : unit -> unit 
  = fun () -> access_mode := true; access := Access.empty; ()

  let return_access : unit -> Access.t
  = fun () -> access_mode := false; !access

  let add k v m =
    (if !access_mode then 
      access := Access.add Access.def k !access);
    add k v m

  let weak_add k v m =
    (if !access_mode then 
      access := Access.add Access.all k !access);
    weak_add k v m

  let find : A.t -> t -> B.t 
  = fun k m ->
     (if !access_mode && not (eq m bot) then 
      access := Access.add Access.use k !access);
    find k m 
end
