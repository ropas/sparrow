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
(** Intra-procedural CFG *)
module Node : sig
  include AbsDom.HASHABLE_SET
  val entry : t
  val exit : t
  val id : t -> int
end

module NodeSet : BatSet.S with type elt = Node.t

module Cmd : sig
  type t =
  | Cinstr of Cil.instr list
  | Cif of Cil.exp * Cil.block * Cil.block * Cil.location
  | CLoop of Cil.location
  (* final graph has the following cmds only *)
  | Cset of Cil.lval * Cil.exp * Cil.location
  | Cexternal of Cil.lval * Cil.location
  | Calloc of Cil.lval * alloc * static * Cil.location
  | Csalloc of Cil.lval * string * Cil.location
  | Cfalloc of Cil.lval * Cil.fundec * Cil.location
  | Cassume of Cil.exp * Cil.location
  | Ccall of Cil.lval option * Cil.exp * Cil.exp list * Cil.location
  | Creturn of Cil.exp option * Cil.location
  | Casm of Cil.attributes * string list *
            (string option * string * Cil.lval) list *
            (string option * string * Cil.exp) list *
            string list * Cil.location
  | Cskip
  and alloc = Array of Cil.exp | Struct of Cil.compinfo
  and static = bool

  val fromCilStmt : Cil.stmtkind -> t
  val to_string : t -> string
end

(** Abstract type of intra-procedural CFG *)
type t
and node = Node.t
and cmd = Cmd.t

val init : Cil.fundec -> Cil.location -> t
val generate_global_proc : Cil.global list -> Cil.fundec -> t

val get_pid : t -> string
val get_formals : t -> Cil.varinfo list
val get_scc_list : t -> node list list

val nodesof : t -> node list
val entryof : t -> node
val exitof : t -> node
val callof : node -> t -> node
val returnof : node -> t -> node

val is_entry : node -> bool
val is_exit : node -> bool
val is_callnode : node -> t -> bool
val is_returnnode : node -> t -> bool
val is_inside_loop : node -> t -> bool

val find_cmd : node -> t ->  cmd

val unreachable_node : t -> NodeSet.t

val compute_scc : t -> t

val optimize : t -> t

val fold_node : (node -> 'a -> 'a) -> t -> 'a -> 'a
val fold_edges : (node -> node -> 'a -> 'a) -> t -> 'a -> 'a

(** {2 Predecessors and Successors } *)

val pred : node -> t -> node list
val succ : node -> t -> node list

(** {2 Graph Manipulation } *)

val add_cmd : node -> cmd -> t -> t
val add_new_node : node -> cmd -> node -> t -> t
val add_node_with_cmd : node -> cmd -> t -> t
val add_edge : node -> node -> t -> t
val remove_node : node -> t -> t

(** {2 Dominators } *)

val compute_dom : t -> t

(** [dom_fronts n g] returns dominance frontiers of node [n] in graph [g] *)
val dom_fronts : node -> t -> NodeSet.t
val children_of_dom_tree : node -> t -> NodeSet.t
val parent_of_dom_tree : node -> t -> node option

(** {2 Print } *)

val print_dot : out_channel -> t -> unit
val to_json : t -> Yojson.Safe.json
