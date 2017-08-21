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
type lib = string
type feature
type data = (lib, feature) BatMap.t
val extract_feature : Global.t -> data
val collect : Global.t -> lib BatSet.t
val print_feature : data -> unit
