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
let analyzer = "../bin/sparrow"
let default_opt = "-verbose 0"

type test_suite = {
  opt: string;
  files: string list;
}

let test_suites = [
  { opt = ""; 
    files = [ "array_pointer.c"; "global_array.c"; "global_static_struct.c"; 
              "global_static_struct2.c"; "local_dynamic_struct.c"; "local_static_struct.c"; 
              "struct_pointer.c"; "test.c" ] } ]
let _ = 
  let files = 
    Sys.readdir "." 
    |> Array.to_list 
    |> List.filter (fun f -> Filename.check_suffix f ".c") 
  in
  List.fold_left (fun c test_suite -> 
    List.fold_left (fun c f ->
      let cmd = analyzer ^ " " ^ default_opt ^ " " ^ test_suite.opt ^ f |> String.split_on_char ' '
              |> Array.of_list in
      let fd = Unix.openfile (f^".out") [Unix.O_CREAT; Unix.O_RDWR] 0o640 in
      let pid = Unix.create_process analyzer cmd Unix.stdin fd fd in
      let _ = Unix.waitpid [] pid in 
      let ic = Unix.open_process_in ("diff " ^ f^".answer " ^ f^".out") in
      Unix.close fd;
      print_string (f^".....");
      match Unix.close_process_in ic with
        Unix.WEXITED i when i = 0 -> print_endline "PASS"; c
      | _ -> print_endline "FAIL"; false
    ) c test_suite.files
  ) true test_suites
  |> (function true -> print_endline "All tests are passed" | false -> print_endline "Test failed");
  List.iter (fun f -> Unix.unlink (f^".out")) files
