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
let make_istr x = 
        "\n\n\n"^
        "--------------------------------------------------------------------------------\n"^
        x^
        "\n--------------------------------------------------------------------------------\n"

let step : bool -> string -> 'a -> ('a -> 'b) -> 'b
 = fun s stg i fn ->
    let t0 = Sys.time() in
    let _ = if s then prerr_endline (make_istr (stg^" begins...")) in
    let _ = if not s then prerr_endline (stg^" begins...") in
    let v = fn i in
    let _ = if s then prerr_endline "" in
    let _ = prerr_endline (stg^" completes: "^(string_of_float (Sys.time()-.t0))) in
        v

let stepf : bool -> string -> ('a -> 'b) -> 'a -> 'b
 = fun s stg fn i ->
  if !Options.opt_verbose >= 1 then 
     step s stg i fn
  else fn i

let stepf_opt : bool -> bool -> string -> ('a -> 'a) -> 'a -> 'a
 = fun b s stg fn i ->
	if b && !Options.opt_verbose >= 1 then step s stg i fn
  else if b then fn i
	else i

let stepf_cond : bool -> bool -> string -> ('a -> 'b) -> ('a -> 'b) -> 'a -> 'b
 = fun b s stg fn1 fn2 i ->
	if b && !Options.opt_verbose >= 1 then step s stg i fn1
  else if not b && !Options.opt_verbose >= 1 then step s stg i fn2
  else if b then fn1 i
	else fn2 i

let stepf_switch : bool -> string -> (bool * ('a -> 'b)) list -> 'a -> 'b
 = fun s stg fn_list i ->
  let fn = List.find fst fn_list |> snd in
	if !Options.opt_verbose >= 1 then step s stg i fn
	else fn i

let stepf_opt_unit : bool -> bool -> string -> ('a -> unit) -> 'a -> unit
= fun b s stg fn i -> if b then step s stg i fn else ()
