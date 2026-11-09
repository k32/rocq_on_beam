(*-----------------------------------------------------------------------*)
(*  Phase 1: translate MiniML (Rocq's extraction term language) into the  *)
(*  Core Erlang AST (Cerlang).                                           *)
(*                                                                       *)
(*  This phase resolves all names: de Bruijn indices (1-based in MiniML) *)
(*  become Core Erlang variables, globals become function references,    *)
(*  and constructors become atoms / tagged tuples. The result contains   *)
(*  no MiniML- or Rocq-specific constructs.                              *)
(*-----------------------------------------------------------------------*)

open Names
module Miniml = Extraction_plugin.Miniml
module Mlutil = Extraction_plugin.Mlutil
module Common = Extraction_plugin.Common
module Extract_env = Extraction_plugin.Extract_env
module Modutil = Extraction_plugin.Modutil
module Table = Extraction_plugin.Table
open Miniml

(* -------------------------------------------------------------------- *)
(* Naming helpers                                                       *)
(* -------------------------------------------------------------------- *)

(* The [atom] type itself is defined in Cerlang (the AST stores atoms).
   Atoms are built here from raw, unprefixed names; quoting and
   escaping happen uniformly at printing time (see [Ceprint.unatom]),
   so they cannot be forgotten or applied twice. *)
type atom = Cerlang.atom

(* The label of a global: for constructors we need the constructor's own
   name, not the inductive's. *)
let label_of_global (r : global) : Id.t =
  match r.glob with
  | GlobRef.ConstructRef ((mind, idx), i) ->
    let mib = Environ.lookup_mind mind (Global.env ()) in
    (try
       let ind = mib.Declarations.mind_packets.(idx) in
       ind.Declarations.mind_consnames.(i - 1)
     with _ -> Table.label_of_r r)
  | _ -> Table.label_of_r r

let global_name (r : global) : atom =
  let s = Id.to_string (label_of_global r) in
  Cerlang.Atom (match InfvInst.encode r.inst with
                | None -> s
                | Some code -> s ^ "_" ^ code)

(* Erasure markers, also atoms: kept distinctive with a coq_ prefix
   since they may collide with user constructor names otherwise *)
let marker_exn = Cerlang.Atom "coq_exn"
let marker_dummy = Cerlang.Atom "coq_dummy"
let marker_axiom = Cerlang.Atom "coq_axiom"

(* -------------------------------------------------------------------- *)
(* Translation context                                                   *)
(* -------------------------------------------------------------------- *)

(* A de Bruijn binder: either a plain variable or (for toplevel fixpoint
   groups) one of the emitted toplevel functions.

   Reminder of MiniML conventions (cf. Common.ml / ocaml.ml):
   - indices are 1-based: [MLrel 1] is the innermost binder;
   - [Mlutil.collect_lams] returns identifiers innermost-first;
   - inside [MLfix (i, ids, defs)], [MLrel n] denotes [ids.(k-n)]. *)
type binder =
  | BVar of Cerlang.var
  | BFun of atom * int  (* function name, arity *)

type ctx = {
  env : binder list;                    (* innermost first *)
  funs : (atom * int) KerName.Map.t;  (* toplevel functions *)
  counter : int ref;
}

let make_ctx funs = { env = []; funs; counter = ref 0 }

let push_binder b ctx = { ctx with env = b :: ctx.env }

let count_lams = let rec c = function
  | MLlam (_, b) -> 1 + c b
  | _ -> 0
in c

(* All Core Erlang variables in a function are identified by unique
   integers from a single per-function counter; the printer turns them
   into names ([_v1], [_v2], ...). Since the counter only ever
   increases, identifiers are unique within the function and can
   neither collide with each other nor capture each other — including
   the eta-expansion parameters, which scope over translated arguments.
   (De Bruijn indices are still resolved through the binder environment;
   the counter only identifies binders.) *)
let fresh_var ctx =
  incr ctx.counter;
  Cerlang.Var !(ctx.counter)

(* -------------------------------------------------------------------- *)
(* Expressions                                                          *)
(* -------------------------------------------------------------------- *)

(* A de Bruijn index, resolved against the binder environment.

   MLrel n (1-based) denotes the n-th enclosing binder:
     - a plain variable  -> the variable name
     - a fixpoint member -> the function reference 'f'/n, which in Core
       Erlang is a first-class value usable with [apply] *)
let trans_var ctx n =
  match List.nth_opt ctx.env (n - 1) with
  | Some (BVar v) -> Cerlang.EVar v
  | Some (BFun (f, arity)) -> Cerlang.EFunRef (f, arity)
  | None -> Cerlang.EVar (Cerlang.Var 0)  (* invalid index; 0 never minted *)

(* Application [MLapp (f, [a1;...;an])]. We first flatten the whole
   application spine (f is itself often an MLapp), then dispatch on the
   head:
     - toplevel global   -> local call  [apply 'f'/n (args)]
                            (eta-expanded if partial, since Core Erlang
                            calls are n-ary, not curried)
     - de Bruijn var     -> [apply H (args)]
     - anything else     -> [apply H (args)] after translating the head *)
let rec trans_apply ctx a =
  let rec spine acc = function
    | MLapp (f, args) -> spine (args @ acc) f
    | MLglob r -> `Glob (r, acc)
    | MLrel n -> `Rel (n, acc)
    | other -> `Dyn (other, acc)
  in
  let open Cerlang in
  match spine [] a with
  | `Glob (r, args) -> trans_global_call ctx r args
  | `Rel (n, args) ->
    EApply (trans_var ctx n, List.map (trans_expr ctx) args)
  | `Dyn (f, args) ->
    EApply (trans_expr ctx f, List.map (trans_expr ctx) args)

(* Call of a toplevel extracted function.
     [MLglob r applied to k arguments]
        -> [apply 'f'/n (a1,...,ak)]        when k >= n
        -> [fun (_vN..) -> apply 'f'/n (a1..ak,_vN..)]
                                            when k < n (partial
                                            application of a curried
                                            Rocq function; Core Erlang
                                            functions are uncurried, so
                                            we eta-expand the missing
                                            arguments)
   If the global is not one of the functions we are emitting (it should
   not happen with monolithic extraction, but be defensive), we emit a
   remote call [call 'module':'name' (args)]. *)
and trans_global_call ctx r args =
  let kn = Table.repr_of_r r in
  let args = List.map (trans_expr ctx) args in
  let open Cerlang in
  match KerName.Map.find_opt kn ctx.funs with
  | Some (fname, arity) when arity > List.length args ->
    let miss = arity - List.length args in
    (* the eta parameters scope over the translated arguments, so they
       must come from the same per-function counter (no capture) *)
    let rec mk_extra n ctx acc =
      if n = 0 then List.rev acc, ctx
      else
        let v = fresh_var ctx in
        mk_extra (n - 1) (push_binder (BVar v) ctx) (v :: acc)
    in
    let extra, _ctx = mk_extra miss ctx [] in
    EFun (extra,
          EApply (EFunRef (fname, arity),
                  args @ List.map (fun v -> EVar v) extra))
  | Some (fname, arity) ->
    EApply (EFunRef (fname, max arity (List.length args)), args)
  | None ->
    ECall (Cerlang.Atom (ModPath.to_string (Table.modpath_of_r r)),
           global_name r, args)

(* A (possibly n-ary) MiniML lambda becomes an uncurried Core Erlang fun:
     [MLlam (x1,...MLlam (xn, body)...)]
        -> [fun (_x1,...,_xn) -> body]
   The nested lambdas are collected with [Mlutil.collect_lams] (which
   returns the identifiers innermost-first) and bound to fresh names. *)
and trans_fun ctx a =
  let ids, body = Mlutil.collect_lams a in
  let ctx, params =
    List.fold_left
      (fun (ctx, params) id ->
        let v = fresh_var ctx in
        (push_binder (BVar v) ctx, v :: params))
      (ctx, []) ids
  in
  Cerlang.EFun (params, trans_expr ctx body)

(* A non-recursive binding:
     [MLletin (x, e1, e2)] -> [let _vN = e1 in e2]
   Note: the value [e1] is translated under the *outer* context (the
   binder is not in scope in its own right-hand side). *)
and trans_let ctx id a1 a2 =
  let v = fresh_var ctx in
  let ctx' = push_binder (BVar v) ctx in
  Cerlang.ELet (v, trans_expr ctx a1, trans_expr ctx' a2)

(* A global in value position (not applied):
     - one of our functions -> the fun reference ['f'/n]
     - otherwise             -> error. With monolithic extraction every
       dependency is part of the structure, so this should not happen;
       there is no sensible Core Erlang to emit for a reference to an
       unextracted constant. *)
and trans_global ctx r =
  let kn = Table.repr_of_r r in
  match KerName.Map.find_opt kn ctx.funs with
  | Some (fname, arity) -> Cerlang.EFunRef (fname, arity)
  | None ->
    CErrors.user_err
      Pp.(str "CoreErlang extraction: global reference to an unextracted constant: "
          ++ str (Id.to_string (Table.label_of_r r)))

(* Constructor application. Inductives have no Core Erlang counterpart,
   so a constructor with arguments becomes a tagged tuple and a nullary
   constructor a plain atom:
     [MLcons (r, [a1;...;an])] -> {'Tag', a1,...,an}
     [MLcons (r, [])]          -> 'Tag'
   Matching code destructures the same representation (see trans_pattern). *)
and trans_cons ctx r args =
  let tag = Cerlang.Atom (Id.to_string (label_of_global r)) in
  let open Cerlang in
  match args with
  | [] -> EAtom tag
  | l -> ETuple (EAtom tag :: List.map (trans_expr ctx) l)

(* A local fixpoint group, denoting its i-th member:
     [MLfix (i, ids, defs)]
        -> [letrec 'f1'/n1 = fun (...) -> ... 'f2'/n2 = ... in 'fi'/ni]
   Inside the group bodies, [MLrel n] denotes [ids.(k-n)], so the binder
   list is [[ids.(k-1);...;ids.(0)]], each entry a BFun pointing at the
   freshly generated name. Recursive calls inside the bodies print as
   [apply 'fj'/nj (...)], which letrec allows. *)
and trans_fix ctx i ids defs =
  let k = Array.length ids in
  let fresh =
    List.init k (fun j ->
      incr ctx.counter;
      (Cerlang.Atom ("fix_" ^ string_of_int !(ctx.counter) ^ "_" ^ string_of_int j),
       count_lams defs.(j)))
  in
  let ctx_group =
    List.fold_left (fun ctx (f, a) -> push_binder (BFun (f, a)) ctx)
      ctx (List.rev fresh)
  in
  let bindings =
    List.mapi
      (fun j def ->
        let idsl, body = Mlutil.collect_lams def in
        let ctx', params =
          List.fold_left
            (fun (ctx, params) id ->
              let v = fresh_var ctx in
              (push_binder (BVar v) ctx, v :: params))
            (ctx_group, []) idsl
        in
        { Cerlang.lr_name = fst (List.nth fresh j);
          lr_params = List.rev params;
          lr_body = trans_expr ctx' body })
      (Array.to_list defs)
  in
  let sel = try List.nth fresh i with _ -> List.hd fresh in
  Cerlang.ELetrec (bindings, Cerlang.EFunRef (fst sel, snd sel))

(* The main dispatch. The structured constructs above have their own
   helpers; what remains are literals and erasure markers:
     - MLuint/MLfloat/MLstring -> Erlang number/string literals
       (Rocq strings become Erlang strings, i.e. integer lists)
     - MLmagic  -> erased (an unsound cast in the target language)
     - MLdummy  -> marker atom 'coq_dummy'   (erased argument)
     - MLexn    -> {'coq_exn', "reason"}     (e.g. unused definitions)
     - MLaxiom  -> {'coq_axiom', "name"}     (logical axiom used as data) *)
and trans_case ctx t branches =
  (* Core Erlang clauses need a guard, so every branch gets [when 'true'].
     The scrutinee is let-bound so that the fallback clause (below) can
     name it. Note: the scrutinee is NOT an implicit binder for the
     branch bodies in MiniML, so the branches are translated under the
     outer context; only the fallback clause mentions the let-bound
     variable. *)
  let v = fresh_var ctx in
  let bs =
    Array.to_list branches
    |> List.map (fun (ids, pat, body) ->
        (* branch idents are outermost-first; [Prel 1] = last *)
        let ctx', names =
          List.fold_left
            (fun (ctx, names) id ->
              let w = fresh_var ctx in
              (push_binder (BVar w) ctx, names @ [w]))
            (ctx, []) ids
        in
        (trans_pattern pat names, trans_expr ctx' body))
  in
  (* a compiler-generated last clause: a non-matching scrutinee raises
     {'badmatch', Scrutinee} instead of a bare case failure *)
  let fallback =
    (Cerlang.PVar v,
     Cerlang.EPrimop (Cerlang.Atom "match_fail",
                      [Cerlang.ETuple [Cerlang.EAtom (Cerlang.Atom "badmatch");
                                       Cerlang.EVar v]]))
  in
  Cerlang.ELet (v, trans_expr ctx t, Cerlang.ECase (Cerlang.EVar v, bs @ [fallback]))

and trans_expr ctx a =
  let open Cerlang in
  match a with
  | MLrel n -> trans_var ctx n
  | MLapp _ -> trans_apply ctx a
  | MLlam _ -> trans_fun ctx a
  | MLletin (id, a1, a2) -> trans_let ctx id a1 a2
  | MLglob r -> trans_global ctx r
  | MLcons (_, r, args) -> trans_cons ctx r args
  | MLtuple l -> ETuple (List.map (trans_expr ctx) l)
  | MLcase (_, t, branches) -> trans_case ctx t branches
  | MLfix (i, ids, defs) -> trans_fix ctx i ids defs
  | MLexn s -> ETuple [EAtom marker_exn; EString s]
  | MLdummy _ -> EAtom marker_dummy
  | MLaxiom s -> ETuple [EAtom marker_axiom; EString s]
  | MLmagic a -> trans_expr ctx a
  | MLuint w -> EInt (Uint63.to_string w)
  | MLfloat f -> EFloat (Float64.to_string f)
  | MLstring s -> EString (Pstring.to_string s)
  | MLparray (arr, _) ->
    (* arrays do not exist in Core Erlang: degraded to a tuple *)
    ETuple (Array.to_list arr |> List.map (trans_expr ctx))

(* Pattern translation, mirroring trans_cons:
     [Pcons (r, ps)]  -> <tagged tuple pattern> / <atom pattern>
     [Pusual r]       -> constructor applied to all its (named) args
     [Ptuple ps]      -> untagged tuple pattern
   [names] holds fresh variables for the branch idents, in ident order;
   [Prel n] denotes the n-th ident from the end (Prel 1 = last). *)
and trans_pattern pat names =
  let open Cerlang in
  let prel n =
    match List.nth_opt names (List.length names - n) with
    | Some v -> PVar v
    | None -> PWild
  in
  match pat with
  | Pwild -> PWild
  | Prel n -> prel n
  | Pcons (r, ps) ->
    let tag = Cerlang.Atom (Id.to_string (label_of_global r)) in
    (match ps with
     | [] -> PAtom tag
     | l -> PTuple (PAtom tag :: List.map (fun p -> trans_pattern p names) l))
  | Ptuple ps -> PTuple (List.map (fun p -> trans_pattern p names) ps)
  | Pusual r ->
    let tag = Cerlang.Atom (Id.to_string (label_of_global r)) in
    (match names with
     | [] -> PAtom tag
     | l -> PTuple (PAtom tag :: List.map (fun v -> PVar v) l))

(* -------------------------------------------------------------------- *)
(* Declarations and whole structures                                     *)
(* -------------------------------------------------------------------- *)

(* One toplevel function in the making: its lambda identifiers
   (outermost-first), the body under the lambdas, the initial
   binder environment (fixpoint group, if any), and whether it is
   exported (listed in the module exports) or internal. *)
type fdef = {
  fd_name : atom;
  fd_ids : ml_ident list;      (* outermost-first *)
  fd_body : ml_ast;            (* body under the lambdas and the group *)
  fd_group : binder list;      (* innermost-first, for MLfix groups *)
  fd_exported : bool;
}

let fundefs_of_decl exported d =
  let is_exported r = KerName.Set.mem (Table.repr_of_r r) exported in
  match d with
  | Dind _ | Dtype _ -> []
  | Dterm (r, body, _) ->
    let ids, body' = Mlutil.collect_lams body in
    [ { fd_name = global_name r; fd_ids = List.rev ids;
        fd_body = body'; fd_group = []; fd_exported = is_exported r } ]
  | Dfix (rv, defs, _) ->
    let k = Array.length rv in
    let arity j = match defs.(j) with
      | MLfix (_, _, inner) -> count_lams inner.(j)
      | b -> count_lams b
    in
    (* binder list for the group: [ids.(k-1);...;ids.(0)], i.e. the same
       order as [rv] reversed; each entry points to the toplevel name *)
    let group = List.init k (fun m -> BFun (global_name rv.(k - 1 - m), arity (k - 1 - m))) in
    List.init k (fun j ->
        let idsl, body' = match defs.(j) with
          | MLfix (_, _, inner) -> Mlutil.collect_lams inner.(j)
          | b -> Mlutil.collect_lams b
        in
        { fd_name = global_name rv.(j);
          fd_ids = List.rev idsl;
          fd_body = body';
          fd_group = group;
          fd_exported = is_exported rv.(j) })

(* Turn a MiniML structure into a Cerlang module:
   1. build the toplevel function map (for call resolution);
   2. translate each declaration's body under its group binders.
   [exported] is the set of kernel names marked [export]; unmarked
   functions are extracted (they may be needed by the exported ones)
   but stay internal, i.e. absent from the module export list. *)
let translate_structure ~(modname : Cerlang.atom) ?(exported = KerName.Set.empty) struc : Cerlang.module_ =
  let funs = ref KerName.Map.empty in
  let add r arity =
    funs := KerName.Map.add (Table.repr_of_r r) (global_name r, arity) !funs
  in
  let decls =
    List.concat_map (fun (_, elems) ->
        List.filter_map (fun (_, elem) ->
            match elem with SEdecl d -> Some d | _ -> None) elems)
      struc
  in
  List.iter (fun d ->
      match d with
      | Dterm (r, body, _) -> add r (count_lams body)
      | Dfix (rv, defs, _) ->
        Array.iteri (fun j r ->
            add r (match defs.(j) with
              | MLfix (_, _, inner) -> count_lams inner.(j)
              | b -> count_lams b)) rv
      | _ -> ())
    decls;
  let ctx0 = make_ctx !funs in
  let funcs =
    List.concat_map (fundefs_of_decl exported) decls
    |> List.map (fun fd ->
        let ctx = { ctx0 with env = fd.fd_group; counter = ref 0 } in
        let ctx, params =
          List.fold_left
            (fun (ctx, params) id ->
              let v = fresh_var ctx in
              (push_binder (BVar v) ctx, v :: params))
            (ctx, []) fd.fd_ids
        in
        { Cerlang.fun_name = fd.fd_name;
          fun_params = List.rev params;
          fun_body = trans_expr ctx fd.fd_body;
          fun_exported = fd.fd_exported })
  in
  { Cerlang.mod_name = modname; mod_funcs = funcs }
