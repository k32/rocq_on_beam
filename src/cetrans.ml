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
  | GlobRef.ConstructRef ((mind, idx), i) -> (
      let mib = Environ.lookup_mind mind (Global.env ()) in
      try
        let ind = mib.Declarations.mind_packets.(idx) in
        ind.Declarations.mind_consnames.(i - 1)
      with _ -> Table.label_of_r r)
  | _ -> Table.label_of_r r

let global_name (r : global) : atom =
  let s = Id.to_string (label_of_global r) in
  Cerlang.Atom
    (match InfvInst.encode r.inst with
    | None -> s
    | Some code -> s ^ "_" ^ code)

(* Erasure markers, also atoms: kept distinctive with a coq_ prefix
   since they may collide with user constructor names otherwise *)
let marker_exn = Cerlang.Atom "coq_exn"
let marker_dummy = Cerlang.Atom "coq_dummy"
let marker_axiom = Cerlang.Atom "coq_axiom"

(* The Core Erlang backend bypasses Rocq's custom-extraction machinery
   (its slots hold target-language source strings, which we would have
   to parse back); native lowerings are hardwired instead. Make any
   user-issued custom extraction fail loudly rather than be silently
   ignored. *)
let check_custom r what =
  if Table.is_custom r || Table.is_inline_custom r || Table.is_foreign_custom r
  then
    CErrors.user_err
      Pp.(
        str "CoreErlang extraction: "
        ++ what
        ++ str " has a custom extraction"
        ++ str " (Extract Constant / Inlined Constant / Inductive), which the"
        ++ str " Core Erlang backend does not support: "
        ++ Id.print (Table.label_of_r r))

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
  | BLam of Cerlang.var * int (* variable bound to an n-ary lambda *)
  | BFun of atom * int (* function name, arity *)

type ctx = {
  env : binder list; (* innermost first *)
  funs : (Cerlang.atom * atom * int) KerName.Map.t;
    (* all extracted functions, module-wide: module name, function
       name, arity *)
  cur_module : Cerlang.atom; (* the module being translated *)
  counter : int ref;
}

let push_binder b ctx = { ctx with env = b :: ctx.env }

let count_lams =
  let rec c = function MLlam (_, b) -> 1 + c b | _ -> 0 in
  c

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

(* the fundef being translated, for diagnostics *)
let current_function = ref ""

(* eta-conversions are correct but inefficient (extra closures) and a
   recurring source of arity mismatches; tell the user, so they can
   refactor towards direct n-ary definitions *)
let eta_info what =
  Feedback.msg_info
    Pp.(
      str "CoreErlang extraction: eta-conversion ("
      ++ str what ++ str ")" ++ str " while translating '"
      ++ str !current_function ++ str "'"
      ++ str "; this is correct but inefficient and error-prone,"
      ++ str " consider refactoring to a direct application")

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
  | Some (BVar v) | Some (BLam (v, _)) -> Cerlang.EVar v
  | Some (BFun (f, arity)) -> Cerlang.EFunRef (f, arity)
  | None -> Cerlang.EVar (Cerlang.Var 0)
(* invalid index; 0 never minted *)

(* -------------------------------------------------------------------- *)
(* The single application engine.                                        *)
(*                                                                       *)
(* MiniML is curried, Core Erlang is not; every callee is described by  *)
(* its uncurried arity (and, for native calls, how many leading ML      *)
(* arguments to drop untranslated), and this one place decides between  *)
(* a direct call, eta-expansion (too few arguments) and a curried       *)
(* split (too many arguments).                                           *)
(* -------------------------------------------------------------------- *)

type callee =
  | Local_fun of Cerlang.atom * int (* one of our fundefs *)
  | Foreign_fun of Cerlang.atom * Cerlang.atom * int
    (* a function of another extracted module: module, name, arity *)
  | Local_var of int * int (* variable, effective arity *)
  | Remote of Cenative.target (* native (or plain remote) call *)

(* translate [drop] arguments away and the rest *)
let rec trans_args ctx drop args =
  if drop = 0 then List.map (trans_expr ctx) args
  else
    match args with
    | _ :: t -> trans_args ctx (drop - 1) t
    | [] ->
        (* [drop] exceeds the argument list: an arity bookkeeping bug
             in the native-call tables *)
        assert false

and take k l =
  if k = 0 then [] else match l with x :: t -> x :: take (k - 1) t | [] -> []

and drop_list k l =
  if k = 0 then l else match l with _ :: t -> drop_list (k - 1) t | [] -> []

(* apply a callee (of known arity [k], ML arguments [args], [ml_drop]
   leading ML arguments dropped untranslated) *)
and apply_callee ctx what render k ml_drop args =
  (* the callee consumes k + ml_drop ML arguments (ml_drop of which are
     dropped untranslated) *)
  let k = k + ml_drop in
  let n = List.length args in
  if n = k then render (trans_args ctx ml_drop args)
  else if n < k then begin
    (* eta-expansion: a partial application must still be a k-ary fun *)
    eta_info ("partial application of " ^ what);
    let miss = k - n in
    let extra = List.init miss (fun _ -> fresh_var ctx) in
    Cerlang.EFun
      ( extra,
        render
          (trans_args ctx ml_drop args
          @ List.map (fun w -> Cerlang.EVar w) extra) )
  end
  else begin
    (* curried overshoot: the callee's own k arguments at once, the
       rest applied to the result one at a time *)
    eta_info (what ^ " applied to more arguments than its arity");
    List.fold_left
      (fun f a -> Cerlang.EApply (Cerlang.CExpr f, [ trans_expr ctx a ]))
      (render (trans_args ctx ml_drop (take k args)))
      (drop_list k args)
  end

(* the callee description of a global, if any *)
and global_callee ctx r =
  match Cenative.global_target r with
  | Cenative.NativeCall _ as t -> Some (Remote t, "a native function", 0)
  | Cenative.NativeCall2 _ as t -> Some (Remote t, "a native function", 0)
  | Cenative.NativeIdentity ->
      Some (Remote Cenative.NativeIdentity, "the identity", 0)
  | Cenative.NativeValue v ->
      Some (Remote (Cenative.NativeValue v), "a native value", 0)
  | Cenative.NotNative -> (
      match KerName.Map.find_opt (Table.repr_of_r r) ctx.funs with
      | Some (m, fname, arity) ->
          if m = ctx.cur_module then
            Some (Local_fun (fname, arity), "an extracted function", 0)
          else
            (* a function of another module being extracted: a remote
               call, with the module path as the module name *)
            Some (Foreign_fun (m, fname, arity), "a foreign function", 0)
      | None -> None)

and render_remote ctx r t args =
  match t with
  | Cenative.NativeCall _ | Cenative.NativeCall2 _ | Cenative.NativeIdentity ->
      Cenative.render_call t args
  | Cenative.NativeValue v ->
      (* a fixed value; only a nullary application makes sense *)
      if args = [] then v
      else
        CErrors.user_err
          Pp.(
            str
              "CoreErlang extraction: a fixed native value applied to arguments")
  | _ -> assert false

(* Application [MLapp (f, [a1;...;an])]: flatten the spine, then
   dispatch on the head *)
and trans_apply ctx a =
  let rec spine acc = function
    | MLapp (f, args) -> spine (args @ acc) f
    | MLmagic f -> spine acc f (* unsafe cast: erased, look through *)
    | MLglob r -> `Glob (r, acc)
    | MLrel n -> `Rel (n, acc)
    | other -> `Dyn (other, acc)
  in
  let open Cerlang in
  match spine [] a with
  | `Glob (r, args) -> (
      match global_callee ctx r with
      | Some (Remote (Cenative.NativeCall (_, _, k, ml_drop) as t), what, _) ->
          apply_callee ctx what (render_remote ctx r t) k ml_drop args
      | Some (Remote (Cenative.NativeCall2 _ as t), what, _) ->
          (* unary ML arity; the synthetic BIF operand is added at render *)
          apply_callee ctx what (render_remote ctx r t) 1 0 args
      | Some (Remote Cenative.NativeIdentity, _, _) ->
          Cenative.render_call Cenative.NativeIdentity
            (List.map (trans_expr ctx) args)
      | Some (Remote ((Cenative.NativeValue _ | Cenative.NotNative) as t), _, _)
        ->
          render_remote ctx r t (List.map (trans_expr ctx) args)
      | Some (Local_fun (fname, arity), what, ml_drop) ->
          apply_callee ctx what
            (fun a -> EApply (CExpr (EFunRef (fname, arity)), a))
            arity ml_drop args
      | Some (Foreign_fun (m, f, arity), what, _) ->
          (* a call into another extracted module *)
          apply_callee ctx what
            (fun a -> EApply (CGlobal (m, f), a))
            arity 0 args
      | Some (Local_var _, _, _) -> assert false
      | None ->
          (* unknown global: a plain remote call *)
          EApply
            ( CGlobal
                ( Cerlang.Atom (ModPath.to_string (Table.modpath_of_r r)),
                  global_name r ),
              List.map (trans_expr ctx) args ))
  | `Rel (n, args) -> (
      match List.nth_opt ctx.env (n - 1) with
      | Some (BLam (v, k)) when k > 0 ->
          apply_callee ctx "a local function"
            (fun a -> EApply (CExpr (EVar v), a))
            k 0 args
      | _ -> EApply (CExpr (trans_var ctx n), List.map (trans_expr ctx) args))
  | `Dyn (f, args) ->
      EApply (CExpr (trans_expr ctx f), List.map (trans_expr ctx) args)

(* a global in value position (not applied) *)
and trans_global ctx r =
  match global_callee ctx r with
  | Some (Remote (Cenative.NativeValue v), _, _) -> v
  | Some (Remote (Cenative.NativeCall (_, _, arity, _) as t), what, _) ->
      eta_info (what ^ " used as a value (closure synthesized)");
      let vars = List.init arity (fun _ -> fresh_var ctx) in
      Cerlang.EFun
        (vars, render_remote ctx r t (List.map (fun w -> Cerlang.EVar w) vars))
  | Some (Remote (Cenative.NativeCall2 _ as t), what, _) ->
      eta_info (what ^ " used as a value (closure synthesized)");
      let v = fresh_var ctx in
      Cerlang.EFun ([ v ], render_remote ctx r t [ Cerlang.EVar v ])
  | Some (Remote Cenative.NativeIdentity, _, _) ->
      let v = fresh_var ctx in
      Cerlang.EFun ([ v ], Cerlang.EVar v)
  | Some (Local_fun (fname, 0), _, _) ->
      (* an arity-0 definition denotes its value, not a function *)
      Cerlang.EApply (Cerlang.CExpr (Cerlang.EFunRef (fname, 0)), [])
  | Some (Local_fun (fname, arity), what, _) ->
      eta_info (what ^ " used as a value (fun reference)");
      Cerlang.EFunRef (fname, arity)
  | Some (Foreign_fun (m, f, arity), what, _) ->
      (* value position: synthesize a closure of the remote call *)
      eta_info (what ^ " used as a value (closure synthesized)");
      let vars = List.init arity (fun _ -> fresh_var ctx) in
      Cerlang.EFun
        ( vars,
          Cerlang.EApply
            (Cerlang.CGlobal (m, f), List.map (fun w -> Cerlang.EVar w) vars) )
  | Some (Local_var _, _, _) -> assert false
  | Some (Remote Cenative.NotNative, _, _) -> assert false
  | None -> (
      (* not one of ours and not native: extraction should have pulled
       every dependency in, so this is an error *)
      match Cenative.call_arity r with
      | Some _ -> assert false (* impossible: it would be a NativeCall *)
      | None ->
          CErrors.user_err
            Pp.(
              str
                "CoreErlang extraction: global reference to an unextracted \
                 constant: "
              ++ str (ModPath.to_string (Table.modpath_of_r r))
              ++ str "."
              ++ str (Id.to_string (Table.label_of_r r))))

(* A (possibly n-ary) MiniML lambda becomes an uncurried Core Erlang fun:
     [MLlam (x1,...MLlam (xn, body)...)]
        -> [fun (_x1,...,_xn) -> body]
   The nested lambdas are collected with [Mlutil.collect_lams] (which
   returns the identifiers innermost-first) and bound to fresh names. *)
and trans_fun ctx a =
  (* [collect_lams] returns identifiers innermost-first; push them
     outermost-first so that the binder environment has the innermost
     binder at its head, as MiniML's 1-based indices require *)
  let ids, body = Mlutil.collect_lams a in
  let ctx, params =
    List.fold_left
      (fun (ctx, params) id ->
        let v = fresh_var ctx in
        (push_binder (BVar v) ctx, v :: params))
      (ctx, []) (List.rev ids)
  in
  let params = List.rev params in
  (* extraction's eta-contraction can produce [fun x1..xk -> f] where f
     is an n-ary function: the trailing parameters were absorbed into
     f. Expand to the full uncurried arity so that higher-order uses
     (e.g. as a maps:fold callback) see one n+k-ary function. The
     reference may be local (a fun reference) or foreign (a synthesized
     closure of a remote call). *)
  let expand_eta callee n what =
    eta_info what;
    let extra = List.init n (fun _ -> fresh_var ctx) in
    Cerlang.EFun
      ( params @ extra,
        Cerlang.EApply (callee, List.map (fun w -> Cerlang.EVar w) extra) )
  in
  match trans_expr ctx body with
  | Cerlang.EFunRef (f, n) when n > 0 ->
      expand_eta (Cerlang.CExpr (Cerlang.EFunRef (f, n))) n
        "lambda reduced to a bare function reference by extraction's \
         eta-contraction, re-expanded"
  | Cerlang.EFun (extra, Cerlang.EApply ((Cerlang.CGlobal _ as c), args))
    when args = List.map (fun w -> Cerlang.EVar w) extra ->
      expand_eta c (List.length extra)
        "lambda reduced to a remote call closure by extraction's \
         eta-contraction, re-expanded"
  | body -> Cerlang.EFun (params, body)

(* A non-recursive binding:
     [MLletin (x, e1, e2)] -> [let _vN = e1 in e2]
   Note: the value [e1] is translated under the *outer* context (the
   binder is not in scope in its own right-hand side). *)
and trans_let ctx id a1 a2 =
  let v = fresh_var ctx in
  (* remember the effective arity of function-valued bindings, be they
     lambdas, function references or partial applications (see
     trans_apply) *)
  let b =
    match effective_arity ctx a1 with
    | Some k when k > 0 -> BLam (v, k)
    | _ -> BVar v
  in
  let ctx' = push_binder b ctx in
  Cerlang.ELet (v, trans_expr ctx a1, trans_expr ctx' a2)

(* the remaining (uncurried) arity of a value, if it is a function *)
and effective_arity ctx a =
  match a with
  | MLmagic t -> effective_arity ctx t
  | MLlam _ as l -> Some (count_lams l)
  | MLglob r -> (
      match Cenative.global_target r with
      | Cenative.NativeCall (_, _, a, drop) -> Some (a + drop)
      | Cenative.NativeCall2 _ -> Some 1
      | Cenative.NativeIdentity -> Some 1
      | _ -> (
          match KerName.Map.find_opt (Table.repr_of_r r) ctx.funs with
          | Some (_, _, ar) -> Some ar
          | None -> None))
  | MLapp _ ->
      let rec spine acc = function
        | MLapp (f, args) -> spine (args @ acc) f
        | MLmagic f -> spine acc f
        | h -> (h, acc)
      in
      let head, args = spine [] a in
      let ha = effective_arity ctx head in
      Option.map (fun h -> h - List.length args) ha
  | MLrel n -> (
      match List.nth_opt ctx.env (n - 1) with
      | Some (BLam (_, k)) -> Some k
      | _ -> None)
  | _ -> None

(* Constructor application [MLcons (r, args)]: builds the value of
   constructor [r] applied to [args] — natively lowered constructors
   are handled by [Cenative]; everything else falls back to the tagged
   encoding ('tag' for nullary, {'tag', args...} otherwise). *)
and trans_constructor ctx r args =
  (* natively lowered constructors are handled by [Cenative];
     everything else falls back to the tagged encoding *)
  if Cenative.is_native_constructor r then
    Cenative.translate_constructor (trans_expr ctx) r args
  else begin
    let open Cerlang in
    let tag = Cerlang.Atom (Id.to_string (label_of_global r)) in
    match args with
    | [] -> EAtom tag
    | l -> ETuple (EAtom tag :: List.map (trans_expr ctx) l)
  end

(* Pattern translation, mirroring trans_constructor:
     [Pcons (r, ps)]  -> <tagged tuple pattern> / <atom pattern>
     [Pusual r]       -> constructor applied to all its (named) args
     [Ptuple ps]      -> untagged tuple pattern
   with the same native exceptions: nil -> [], cons -> [P|P],
   pair -> untagged tuple.
   [names] holds fresh variables for the branch idents, in ident order;
   [Prel n] denotes the n-th ident from the end (Prel 1 = last). *)
and trans_case ctx t branches =
  (* Core Erlang clauses need a guard, so every branch gets [when ...]
     (usually just ['true']). The scrutinee is let-bound so that the
     fallback clause (below) can name it. Note: the scrutinee is NOT an
     implicit binder for the branch bodies in MiniML, so the branches
     are translated under the outer context; only the fallback clause
     mentions the let-bound variable.

     Native lowering of matches is decided by [Cenative.is_native_branch]:
     if any branch cannot be rewritten, the whole match keeps its
     (tagged) translation and will fail loudly with badmatch at
     runtime should it be reached. *)
  let v = fresh_var ctx in
  if Table.is_custom_match branches then
    CErrors.user_err
      Pp.(
        str "CoreErlang extraction: this match has a custom match extraction"
        ++ str " (Extract Inductive ... with a match function), which the"
        ++ str " Core Erlang backend does not support");
  let guard_true = Cenative.guard_true in
  let bs =
    Array.to_list branches
    |> List.map (fun (ids, pat, body) ->
        (* branch idents are outermost-first; [Prel 1] = last *)
        let ctx', names =
          List.fold_left
            (fun (ctx, names) id ->
              let w = fresh_var ctx in
              (push_binder (BVar w) ctx, w :: names))
            (ctx, []) ids
        in
        let names = List.rev names in
        let name = match names with [ x ] -> Some x | _ -> None in
        if Cenative.is_native_branch pat && List.length ids <= 1 then begin
          let p, g, binders = Cenative.translate_branch v pat name in
          let body = trans_expr ctx' body in
          let body =
            List.fold_left (fun b (w, e) -> Cerlang.ELet (w, e, b)) body binders
          in
          (p, g, body)
        end
        else (trans_pattern pat names, guard_true, trans_expr ctx' body))
  in
  (* a compiler-generated last clause: a non-matching scrutinee raises
     {'badmatch', Scrutinee} instead of a bare case failure *)
  let fallback =
    ( Cerlang.PVar v,
      guard_true,
      Cerlang.EApply
        ( Cerlang.CPrimop (Cerlang.Atom "match_fail"),
          [
            Cerlang.ETuple
              [ Cerlang.EAtom (Cerlang.Atom "badmatch"); Cerlang.EVar v ];
          ] ) )
  in
  Cerlang.ELet
    (v, trans_expr ctx t, Cerlang.ECase (Cerlang.EVar v, bs @ [ fallback ]))

and trans_fix ctx i ids defs =
  let k = Array.length ids in
  let fresh =
    List.init k (fun j ->
        incr ctx.counter;
        ( Cerlang.Atom
            ("fix_" ^ string_of_int !(ctx.counter) ^ "_" ^ string_of_int j),
          count_lams defs.(j) ))
  in
  let ctx_group =
    List.fold_left
      (fun ctx (f, a) -> push_binder (BFun (f, a)) ctx)
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
        {
          Cerlang.lr_name = fst (List.nth fresh j);
          lr_params = List.rev params;
          lr_body = trans_expr ctx' body;
        })
      (Array.to_list defs)
  in
  let sel =
    match List.nth_opt fresh i with Some s -> s | None -> assert false
  in
  Cerlang.ELetrec (bindings, Cerlang.EFunRef (fst sel, snd sel))

and trans_expr ctx a =
  let open Cerlang in
  match a with
  | MLrel n -> trans_var ctx n
  | MLapp _ -> trans_apply ctx a
  | MLlam _ -> trans_fun ctx a
  | MLletin (id, a1, a2) -> trans_let ctx id a1 a2
  | MLglob r -> trans_global ctx r
  | MLcons (_, r, args) -> trans_constructor ctx r args
  | MLtuple l -> ETuple (List.map (trans_expr ctx) l)
  | MLcase (_, t, branches) -> trans_case ctx t branches
  | MLfix (i, ids, defs) -> trans_fix ctx i ids defs
  | MLexn s -> ETuple [ EAtom marker_exn; EString s ]
  | MLdummy _ -> EAtom marker_dummy
  | MLaxiom s -> ETuple [ EAtom marker_axiom; EString s ]
  | MLmagic a -> trans_expr ctx a
  | MLuint w -> EInt (Z.of_int64 (Uint63.to_int64 w))
  | MLfloat f -> EFloat (Float64.to_float f)
  | MLstring s -> EBinary (Pstring.to_string s) (* native: binary *)
  | MLparray (arr, _) ->
      (* arrays do not exist in Core Erlang: degraded to a tuple *)
      ETuple (Array.to_list arr |> List.map (trans_expr ctx))

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
      if Cenative.is_native_pattern r then
        Cenative.translate_pattern (fun p -> trans_pattern p names) r ps
      else begin
        (* tagged fallback *)
        let tag = Cerlang.Atom (Id.to_string (label_of_global r)) in
        match ps with
        | [] -> PAtom tag
        | l -> PTuple (PAtom tag :: List.map (fun p -> trans_pattern p names) l)
      end
  | Ptuple ps -> PTuple (List.map (fun p -> trans_pattern p names) ps)
  | Pusual r ->
      (* constructor applied to all its (named) args *)
      if Cenative.is_native_usual r then
        Cenative.translate_pattern_usual r names
      else begin
        let tag = Cerlang.Atom (Id.to_string (label_of_global r)) in
        match names with
        | [] -> PAtom tag
        | l -> PTuple (PAtom tag :: List.map (fun v -> PVar v) l)
      end
