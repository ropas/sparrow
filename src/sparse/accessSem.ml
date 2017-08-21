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
open AbsSem
open BasicDom
open ItvDom

module type S =
sig
  include AbsSem.S
  module Access : Access.S with type t = Dom.Access.t
    and type Info.t = Dom.Access.Info.t
  val accessof : ?locset: Dom.PowA.t -> Global.t -> BasicDom.Node.t
    -> (BasicDom.Node.t -> Dom.t * Global.t -> Dom.t * Global.t) -> Dom.t
    -> Access.info
end

module Make (Sem : AbsSem.S) =
struct
  include Sem
  module Access = Sem.Dom.Access
  let accessof ?(locset=Dom.PowA.empty): Global.t -> Node.t -> (Node.t -> Dom.t * Global.t -> Dom.t * Global.t) -> Dom.t -> Dom.Access.info
  = fun global node f mem ->
    Dom.init_access ();
    let _ = f node (mem,global) in
    Dom.return_access ()
end
