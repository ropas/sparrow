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
              "struct_pointer.c"; "test.c" ] };
  { opt = "-narrow";
    files = [ "narrow.c" ] };
  { opt = "-inline alloc -unsound_alloc";
    files = [ "unsound_alloc.c" ] } ]

let _ = 
  List.fold_left (fun c test_suite -> 
    List.fold_left (fun c f ->
      let cmd = analyzer ^ " " ^ default_opt ^ " " ^ test_suite.opt ^ " " ^ f
              |> Str.split (Str.regexp "[ ]+") 
              |> Array.of_list in
      (try Unix.unlink (f ^ ".out") with _ -> ());
      let fd = Unix.openfile (f^".out") [Unix.O_CREAT; Unix.O_RDWR] 0o640 in
      let pid = Unix.create_process analyzer cmd Unix.stdin fd fd in
      let _ = Unix.waitpid [] pid in 
      let ic = Unix.open_process_in ("diff " ^ f^".answer " ^ f^".out") in
      Unix.close fd;
      print_string (f^".....");
      match Unix.close_process_in ic with
        Unix.WEXITED i when i = 0 -> print_endline "PASS"; Unix.unlink (f^".out"); c
      | _ -> print_endline ("FAIL (see "^ f ^ ".answer and " ^ f ^ ".out)"); false
    ) c test_suite.files
  ) true test_suites
  |> (function true -> print_endline "All tests are passed" | false -> print_endline "Test failed")
