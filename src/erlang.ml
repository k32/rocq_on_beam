open Extraction_plugin
open Pp
open CErrors
open Util
open Names
open Table
open Miniml
open Mlutil
open Common
open Erlang_ast

let keywords =
  List.fold_right
    (fun s -> Id.Set.add (Id.of_string s))
    [
      "after";
      "and";
      "andalso";
      "band";
      "begin";
      "bnot";
      "bor";
      "bsl";
      "bsr";
      "bxor";
      "case";
      "catch";
      "cond";
      "div";
      "else";
      "end";
      "fun";
      "if";
      "let";
      "maybe";
      "not";
      "of";
      "or";
      "orelse";
      "receive";
      "rem";
      "try";
      "when";
      "xor";
    ]
    Id.Set.empty

(* FIXME: These two guys should be gone in the final product *)
let unsup what = ELit (EAtom (Atom ("Not supported: " ^ what)))

let panic what = CErrors.user_err Pp.(what)

(* Format constructor name for debugging *)
let mlconstr_name = function
  | MLcons (_, _, _) -> "MLcons"
  | MLtuple _ -> "MLtuple"
  | MLuint _ -> "MLuint"
  | MLfloat _ -> "MLfloat"
  | MLletin (_, _, _) -> "MLletin"
  | MLapp (_, _) -> "MLapp"
  | MLcase (_, _, _) -> "MLcase"
  | MLrel _ -> "MLrel"
  | MLglob _ -> "MLglob"
  | MLfix  (_, _, _) -> "MLfix"
  | MLlam (_, _) -> "MLlam"
  | MLexn _ -> "MLexn"
  | MLdummy _ -> "MLdummy"
  | MLaxiom _ -> "MLaxiom"
  | MLmagic _ -> "MLmagic"
  | MLstring _ -> "MLstring"
  | MLparray (_, _) -> "MLparray"

(* Key-value list used to lookup arity of locally defined lambdas.
   Arguments of the top-level definition are [None],
   as we don't have the info about them.
   [None] skips the arity check and eta-conversion,
   and assumes that arity of the callback = number of the applied arguments,
   i.e. the value is properly curried *)
type loc_arity = (var * int option) list

(* Context used during translation of top-level functions *)
type ctx = {
    (* Arities of previously extracted top-level definitions *)
    top_arity : int KerName.Map.t;
    (* Local lambda arities *)
    local_arity : loc_arity ref;
    (* Function that is being extracted: *)
    this_function : global;
    (* Counter used to add prefix to the new variables.
       It is mutable as to make all variables unique,
       regadless of the AST traversal method.
       This is critical to avoid unintended variable capture in the extracted Erlang,
       because Erlang scoping rules are weird. *)
    var_ctr : int ref;
    (* Variables in the scope, for resolving de Brujin indices. Innermost first *)
    scope : var list;
  }

(* Resolve a de Brujin index into a variable name *)
let lookup_var {scope = sc} idx =
  let idx = idx - 1 in (* de Brujin indices in MiniML are 1-based *)
  match List.nth_opt sc idx with
  | Some v ->
     v
  | None ->
     panic (str "de Brujin index overflow: " ++ int idx ++
              str " in scope " ++ prlist_with_sep spc pr_var sc)

let mlvar_to_name = function
  | Dummy ->
     ""
  | Id i | Tmp i ->
     let str = Id.to_string i in
     if String.for_all Char.Ascii.is_alphanum str then
       str
     else
       ""

let ctx_fresh_id {var_ctr = ctr} =
  let fresh = !ctr in
  ctr := fresh + 1;
  fresh

(* Iterate over nested lambdas, adding variables into the context.
   Return the updated context, list of variables in the Erlang AST format (outermost first)
   and the body of the innermost lambda *)
let push_lams ctx ast =
  let rec go ctx vars = function
    | MLlam (id, ast) ->
       let fresh = ctx_fresh_id ctx in
       let evar = Var (mlvar_to_name id, fresh) in
       let ctx = {ctx with scope = evar :: ctx.scope} in
       go ctx (evar :: vars) ast
    | ast ->
       (vars, ctx, ast) in
  let (vars, ctx, ast) = go ctx [] ast in
  (ctx, List.rev vars, ast)

(* Add a new variable into the context *)
let push_var id ctx =
  let fresh = ctx_fresh_id ctx in
  let evar = Var (mlvar_to_name id, fresh) in
  ( evar
  , {ctx with scope = evar :: ctx.scope}
  )

let rec push_vars ctx vars =
  let rec go ctx acc = function
    | [] ->
       (acc, ctx)
    | v :: l ->
       let (ev, ctx) = push_var v ctx in
       go ctx (ev :: acc) l in
  let (evars, ctx) = go ctx [] vars in
  (List.rev evars, ctx)

let push_fix ctx =
  let fresh = ctx_fresh_id ctx in
  let evar = Var ("@fix", fresh) in
  ( evar,
    {ctx with scope = evar :: ctx.scope}
  )

let label_of_global (r : global) : Id.t =
  match r.glob with
  | GlobRef.ConstructRef ((mind, idx), i) -> (
      let mib = Environ.lookup_mind mind (Global.env ()) in
      try
        let ind = mib.Declarations.mind_packets.(idx) in
        ind.Declarations.mind_consnames.(i - 1)
      with _ -> Table.label_of_r r)
  | _ ->
     Table.label_of_r r

let global_name (r : global) =
  let s = Id.to_string (label_of_global r) in
  match InfvInst.encode r.inst with
  | None -> s
  | Some code -> s ^ "_" ^ code

let push_arity gl ast =
  let rec go acc = function
    | MLlam (_, ast) ->
       go (acc + 1) ast
    | _ ->
       acc in
  let arity = go 0 ast in
  KerName.Map.add (Table.repr_of_r gl) arity

let push_loc_arity var arity {local_arity = l} =
  l := (var, arity) :: !l

let gl_arity {top_arity = m} gl =
  match KerName.Map.find_opt (Table.repr_of_r gl) m with
  | Some a ->
     a
  | None ->
     panic (str "Reference to an unknown top-level function " ++ str (global_name gl))

let loc_arity ctx idx =
  let var = lookup_var ctx idx in
  let l = !(ctx.local_arity) in
  match List.find_opt (fun (i, _) -> i = var) l with
  | Some (_, a) ->
     a
  | None ->
     panic (str "Reference to an unknown local function " ++ pr_var var ++
              str ". Bound: " ++ prlist_with_sep spc (fun (v, _) -> pr_var v) l)

let is_local {this_function = this} gl =
  let m = Table.modpath_of_r gl in
  if ModPath.equal (Table.modpath_of_r this) m then
    None
  else
    Some m

(* Translate a match pattern.
   [evars] is a list of Erlang variables bound by the original top level branch *)
let rec tr_pattern ctx evars = function
  | Pcons (gl, args) ->
     let tag = EPLit (EAtom (Atom (Id.to_string (label_of_global gl)))) in
     (match args with
      | [] ->
         tag
      | l ->
         EPTuple (tag :: List.map (tr_pattern ctx evars) l))
  | Ptuple elems ->
     EPTuple (List.map (tr_pattern ctx evars) elems)
  | Prel idx ->
     EPVar (lookup_var ctx idx)
  | Pwild ->
     EPWildcard
  | Pusual gl ->
     let tag = EPLit (EAtom (Atom (global_name gl))) in
     match evars with
     | [] ->
        tag
     | l ->
        EPTuple (tag :: List.map (fun i -> EPVar i) l)

let rec ml_to_erlang table ctx bound_to all_ast =
  match all_ast with
  | MLcons (tp, gl, args) ->
     tr_constructor table ctx gl args
  | MLtuple elems ->
     ETuple (List.map (ml_to_erlang table ctx None) elems)
  | MLuint int ->
     ELit (EInt (Z.of_int64 (Uint63.to_int64 int)))
  | MLfloat netyx ->
     ELit (EFloat netyx)
  | MLletin (ident, val_ast, in_ast) ->
     let (v, ctx') = push_var ident ctx in
     let value = ml_to_erlang table ctx (Some v) val_ast in
     ELet (v, value, ml_to_erlang table ctx' None in_ast)
  | MLapp (callee, args) ->
     tr_app table ctx callee args
  | MLcase (tp, scrutinee, branches) ->
     tr_case table ctx bound_to tp scrutinee branches
  | MLrel idx ->
     EVar (lookup_var ctx idx)
  | MLglob gl ->
     tr_global ctx gl
  | MLfix  (i, ids, defs) ->
     unsup "MLfix"
  | MLlam (_, _) ->
     tr_lam table ctx bound_to all_ast
  | MLexn str ->
     unsup "MLexn"
  | MLdummy kill_reason ->
     unsup "MLdummy"
  | MLaxiom str ->
     unsup "MLaxiom"
  | MLmagic ast ->
     unsup "MLmagic"
  | MLstring pstr ->
     unsup "MLstring"
  | MLparray (asts, ast) ->
     unsup "MLparray"

and tr_case table ctx bound_to tp scrutinee branches =
  ECase (ml_to_erlang table ctx None scrutinee,
         List.map (tr_branch table ctx) (Array.to_list branches))

and tr_lam table ctx bound_to ast =
  let (ctx, vars, ast) = push_lams ctx ast in
  (match bound_to with
   | None ->
      ()
   | Some var ->
      push_loc_arity var (Some (List.length vars)) ctx);
  EFun (vars, ml_to_erlang table ctx None ast)

and tr_global ctx gl =
  (* TODO: distinguish global / local fun references - this is
     semantically important in Erlang, as it affects hot code swap.
     Possibly introduce a pseudo-BIF? *)
  let f = global_name gl in
  let a = gl_arity ctx gl in
  match is_local ctx gl with
  | Some m ->
     ELit (EFunRefG (Atom (ModPath.to_string m), Atom f, a))
  | None ->
     ELit (EFunRefL (Atom f, a))

and tr_branch table ctx (vars, pat, ast) =
  let (evars, ctx) = push_vars ctx vars in
  ( tr_pattern ctx evars pat
  , ml_to_erlang table ctx None ast
  )

and tr_app table ctx callee args =
  let (callee, arity) =
    match callee with
    | MLglob gl ->
       let arity = gl_arity ctx gl in
       (match is_local ctx gl with
        | None ->
           let t = (Atom (global_name gl), arity) in
           (ECallL t, arity)
        | Some m ->
           let t = (Atom (ModPath.to_string m),
                    Atom (global_name gl),
                    arity) in
           (ECallG t, arity))
    | MLrel var ->
       let arity = (match loc_arity ctx var with
                    | Some a ->
                       a
                    | None ->
                       (* This is a callback passed in an argument of the toplevel definition.
                          We don't have the data to check its arity -
                          *assuming* it to be properly uncurried. *)
                       List.length args) in
       (ECallV (lookup_var ctx var), arity)
    | what ->
       panic (str "Unsupported type of callee " ++ str (mlconstr_name what))
  in
  (* FIXME: eta conversion *)
  EApply (callee, List.map (ml_to_erlang table ctx None) args)

and tr_constructor table ctx gl args =
  let tag = ELit (EAtom (Atom (Id.to_string (label_of_global gl)))) in
  match args with
  | [] ->
     tag
  | l ->
     ETuple (tag :: List.map (ml_to_erlang table ctx None) l)

let decl_to_erlang ar_map table gl ast tp =
  let name = label_of_global gl in
  let ctx = { top_arity = ar_map;
              local_arity = ref [];
              this_function = gl;
              var_ctr = ref 0;
              scope = []
            } in
  let (ctx, vars, ast) = push_lams ctx ast in
  List.iter (fun v -> push_loc_arity v None ctx) vars;
  { name = Atom (Id.to_string name);
    args = vars;
    ast = ml_to_erlang table ctx None ast
  }

let pp_top_fun ar_map table gl ast tp =
  let name = pp_global_name table Term gl in
  if is_custom gl
  then
    CErrors.user_err Pp.(str "Cannot extract native function " ++ str name)
  else
    let east = decl_to_erlang ar_map (State.get_table table) gl ast tp in
    v 0 (pr_toplevel east ++ brk (0, -1000))

let pp_decl table = function
  | Dind _ ->
     panic (str "Extraction of inductives is not supported")
  | Dtype _ ->
     panic (str "Extraction of types is not supported")
  | Dterm (gl, ast, tp) ->
     let ar_map = push_arity gl ast KerName.Map.empty in
     pp_top_fun ar_map table gl ast tp
  | Dfix (gls, asts, tps) ->
     let f (i, acc) gl = (i + 1, push_arity gl asts.(i) acc) in
     let (_, ar_map) = Array.fold_left f (0, KerName.Map.empty) gls in
     let f i gl = pp_top_fun ar_map table gl asts.(i) tps.(i) in
     let sep _ = brk (0, -1000) ++ cut () in
     prvecti_with_sep sep f gls

let file_naming state mp = file_of_modfile (State.get_table state) mp

let id_of_filename f =
  try Id.of_string f
  with CErrors.UserError _ ->
    panic (str "Extraction: provided filename is not a valid identifier: " ++ quote (qstring f))

let preamble table mod_name comment used_modules usf = str ""

let pp_struct table stcts = str "TODO"

let erlang_descr =
  {
    keywords;
    file_suffix = ".erl";
    file_naming = file_naming;
    preamble;
    pp_struct;
    sig_suffix = None;
    sig_preamble = (fun _ _ _ _ _ -> mt ());
    pp_sig = (fun _ _ -> mt ());
    pp_decl = pp_decl;
    unquote = (fun s -> s);
    upper_types = false;
    char_type = Some "binary()";
    string_type = Some "binary()";
    modular = true;
    id_of_filename = id_of_filename;
  }

let () = register_language (External "Erlang") erlang_descr
