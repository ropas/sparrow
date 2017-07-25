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
  include AbsSem.S
  module Access : Access.S with type t = Dom.Access.t
    and type Info.t = Dom.Access.Info.t
  val accessof : ?locset: Dom.PowA.t -> Global.t -> BasicDom.Node.t
    -> (BasicDom.Node.t -> Dom.t * Global.t -> Dom.t * Global.t) -> Dom.t
    -> Access.info
end

module Make (Sem : AbsSem.S) : S 
  with type Dom.t = Sem.Dom.t
  and type Dom.A.t = Sem.Dom.A.t 
  and type Dom.PowA.t = Sem.Dom.PowA.t
  and type Dom.Access.t = Sem.Dom.Access.t
  and type Dom.Access.Info.t = Sem.Dom.Access.Info.t
