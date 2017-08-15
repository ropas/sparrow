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
let output : string -> 'a -> unit
= fun name data ->
  let chan = open_out (!Options.marshal_dir ^ "/" ^ name) in
  Marshal.to_channel chan data [];
  close_out chan

let input : string -> 'a
= fun name -> 
  let chan = open_in (!Options.marshal_dir ^ "/" ^ name) in
  let data = Marshal.from_channel chan in
  close_in chan;
  data
