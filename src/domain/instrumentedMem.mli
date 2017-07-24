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
  val return_access : unit -> Access.info
end 

module Make (Mem : MapDom.CPO) : S 
  with type t = Mem.t 
  and type A.t = Mem.A.t 
  and type B.t = Mem.B.t
  and type PowA.t = Mem.PowA.t
