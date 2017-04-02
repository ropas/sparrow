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
let _ = 
  let files = 
    Sys.readdir "." 
    |> Array.to_list 
    |> List.filter (fun f -> Filename.check_suffix f ".c") 
  in
  List.fold_left (fun c f -> 
    let fd = Unix.openfile (f^".out") [Unix.O_CREAT; Unix.O_RDWR] 0o640 in
    let pid = Unix.create_process "../bin/sparrow" [|"../bin/sparrow"; "-verbose"; "0"; f|] Unix.stdin fd fd in
    let _ = Unix.waitpid [] pid in 
    let ic = Unix.open_process_in ("diff " ^ f^".answer " ^ f^".out") in
    Unix.close fd;

    print_string (f^".....");
    match Unix.close_process_in ic with
      Unix.WEXITED i when i = 0 -> print_endline "PASS"; c
    | _ -> print_endline "FAIL"; false
  ) true files
  |> (function true -> print_endline "All tests are passed" | false -> print_endline "Test failed");
  List.iter (fun f -> Unix.unlink (f^".out")) files
