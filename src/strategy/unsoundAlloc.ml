open Cil
open Vocab

let allocs = ["malloc"; "calloc"; "realloc"]

(* Get the pointer for the allocated block *)
let rec ptr_of_exp op exp =
  match exp with
  | BinOp (op0, CastE (_, e1), e2, t)
  | BinOp (op0, e1, CastE (_, e2), t) when op = op0 ->
    ptr_of_exp op (BinOp (op0, e1, e2, t))
  | BinOp (op0, Lval (Var v, NoOffset), zero, t)
    when op = op0 && isZero zero -> Some v.vname
  | Lval (Var v, NoOffset) when op = Ne -> Some v.vname
  | UnOp (LNot, Lval (Var v, NoOffset), _) when op = Eq -> Some v.vname
  | _ -> None

let is_alloc_ptr_instr instr p =
  match List.rev instr with
  | (Call (Some (Var v, NoOffset), Lval (Var f, NoOffset), _, _))::_ ->
    List.mem f.vname allocs && v.vname = p
  | _ -> false

let rec is_alloc_ptr stmt p =
  match stmt.preds with
  | [pred] ->
    (match pred.skind with
     | Instr [Set ((Var v, NoOffset), Lval (Var tmp, NoOffset), _)]
     | Instr [Set ((Var v, NoOffset), CastE (_, Lval (Var tmp, NoOffset)), _)]
       when p = v.vname -> is_alloc_ptr pred tmp.vname
     | Instr instr -> is_alloc_ptr_instr instr p
     | _ -> false)
  | _ -> false

let check_null op stmt exp =
  match ptr_of_exp op exp with
  | Some p -> is_alloc_ptr stmt p
  | None -> false

class allocVisitor () = object(self)
  inherit nopCilVisitor
  method vstmt (s: stmt) =
    match s.skind with
    | If (e, _, fb, _) when check_null Eq s e ->
      if List.length fb.bstmts = 1 then
        ChangeDoChildrenPost (List.hd fb.bstmts, id)
      else
        ChangeDoChildrenPost (mkStmt (Block fb), id)
    | If (e, tb, _, _) when check_null Ne s e ->
      if List.length tb.bstmts = 1 then
        ChangeDoChildrenPost (List.hd tb.bstmts, id)
      else
        ChangeDoChildrenPost (mkStmt (Block tb), id)
    | _ -> DoChildren
end

let rec split_dead stmts dead =
  match stmts with
  | h::t when h.labels = [] -> split_dead t (h::dead)
  | h::t when h.labels <> [] -> (List.rev dead, stmts)
  | _ -> (List.rev dead, stmts)

let rec split stmts (live, dead) =
  match stmts with
  | h::t ->
    (match h.skind with
     | Return (_, loc) ->
       let (dead', live') = split_dead t [] in
       ((List.rev (h::live)) @ live', dead')
     | _ -> split t (h::live, dead))
  | _ -> (List.rev live, dead)

(* Remove statements after returns. For example,
 * p = malloc(s); if (p != 0) { return p; } return abort();
 * => p = malloc(s); return p; return abort();
 * => p = malloc(s); return p; 
 * *)
class returnVisitor () = object(self)
  inherit nopCilVisitor
  method vblock (b: block) =
    let (live, dead) = split b.bstmts ([], []) in
    match dead with
    | [] -> DoChildren
    | _::_ -> ChangeDoChildrenPost ({ b with bstmts = live }, id)
end

let transform file =
  let vis = new allocVisitor () in
  ignore(Cil.visitCilFile vis file);
  let vis = new returnVisitor () in
  ignore(Cil.visitCilFile vis file);
  Frontend.makeCFGinfo file
