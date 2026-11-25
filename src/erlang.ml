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

type scope_var = {
    evar : var
}

type ctx = {
    (* Arities of previous top-level definitions *)
    top_arity : int KerName.Map.t;
    this_module : ModPath.t;
    var_ctr : int;
    (* Variables in the scope, for resolving de Brujin indices. Innermost first *)
    scope : scope_var list;
  }

let lookup_var {scope = sc} idx =
  let idx = idx - 1 in (* de Brujin indices in MiniML are 1-based *)
  match List.nth_opt sc idx with
  | Some v ->
     v
  | None ->
     panic (str "de Brujin index " ++ int idx)

let mlvar_to_name = function
  | Dummy -> ""
  | Id i | Tmp i -> Id.to_string i

(* Iterate over nested lambdas, adding variables into the context.
   Return arity (number of nested lambdas), updated context and the body *)
let push_lams =
  let rec go arity ctx = function
    | MLlam (id, ast) ->
       (match ctx with
          {var_ctr = ctr; scope = scope} ->
          let new_var = {evar = Var (mlvar_to_name id, ctr)} in
          let ctx = {ctx with
                      var_ctr = ctr + 1;
                      scope = new_var :: scope} in
          go (arity + 1) ctx ast)
    | ast ->
       (arity, ctx, ast) in
  go 0

let push_var id ctx =
  match ctx with
    {var_ctr = ctr; scope = scope} ->
    let evar = Var (mlvar_to_name id, ctr) in
    ( evar
    , {ctx with
        var_ctr = ctr + 1;
        scope = {evar = evar} :: scope}
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
  match ctx with
    {var_ctr = ctr; scope = scope} ->
     let evar = Var ("@fix", ctr) in
     ( evar,
       {ctx with
         var_ctr = ctr + 1;
         scope = {evar = evar} :: scope
       }
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

let gl_arity {top_arity = m} gl =
  match KerName.Map.find_opt (Table.repr_of_r gl) m with
  | Some a ->
     a
  | None ->
     panic (str "Reference to an unknown top-level function " ++ str (global_name gl))

let is_local {this_module = this} gl =
  let m = Table.modpath_of_r gl in
  if ModPath.equal this m then
    None
  else
    Some m

(* Translate match pattern.
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
     EPVar (lookup_var ctx idx).evar
  | Pwild ->
     EPWildcard
  | Pusual gl ->
     let tag = EPLit (EAtom (Atom (global_name gl))) in
     match evars with
     | [] ->
        tag
     | l ->
        EPTuple (tag :: List.map (fun i -> EPVar i) l)

let rec ml_to_erlang table ctx = function
  | MLcons (tp, gl, args) ->
     trans_constructor table ctx gl args
  | MLtuple elems ->
     ETuple (List.map (ml_to_erlang table ctx) elems)
  | MLletin (ident, value, ast) ->
     let value = ml_to_erlang table ctx value in
     let (v, ctx) = push_var ident ctx in
     ELet (v, value, ml_to_erlang table ctx ast)
  | MLapp (callee, args) ->
     trans_app table ctx callee args
  | MLcase (tp, scrutinee, branches) ->
     ECase (ml_to_erlang table ctx scrutinee,
            List.map (trans_branch table ctx) (Array.to_list branches))
  | MLrel idx ->
     EVar (lookup_var ctx idx).evar
  | MLglob gl ->
     tr_global ctx gl
  | MLfix  (i, ids, defs) ->
     unsup "MLfix"
  | MLlam  (ident, ast) ->
     unsup "MLlam"
  | MLexn str ->
     unsup "MLexn"
  | MLdummy kill_reason ->
     unsup "MLdummy"
  | MLaxiom str ->
     unsup "MLaxiom"
  | MLmagic ast ->
     unsup "MLmagic"
  | MLuint int ->
     unsup "MLuint"
  | MLfloat netyx ->
     unsup "MLfloat"
  | MLstring pstr ->
     unsup "MLstring"
  | MLparray (asts, ast) ->
     unsup "MLparray"

and tr_global ctx gl =
  (* TODO: distinguish global / local fun references - important in
     Erlang. Get arity from somewhere *)
  let m = ModPath.to_string (Table.modpath_of_r gl) in
  let f = global_name gl in
  let a = gl_arity ctx gl in
  ELit (EFunRefG (Atom m, Atom f, a))

and trans_branch table ctx (vars, pat, ast) =
  let (evars, ctx) = push_vars ctx vars in
  ( tr_pattern ctx evars pat
  , ml_to_erlang table ctx ast
  )

and trans_app table ctx callee args =
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
       (* FIXME: arity *)
       let arity = List.length args in
       (ECallV (lookup_var ctx var).evar, arity)
    | _ ->
       panic (str "Unknown apply")
  in
  (* FIXME: eta conversion *)
  EApply (callee, List.map (ml_to_erlang table ctx) args)

and trans_constructor table ctx gl args =
  let tag = ELit (EAtom (Atom (Id.to_string (label_of_global gl)))) in
  match args with
  | [] ->
     tag
  | l ->
     ETuple (tag :: List.map (ml_to_erlang table ctx) l)

let decl_to_erlang ar_map table gl ast tp =
  let name = label_of_global gl in
  let thism = Table.modpath_of_r gl in
  let ctx = { top_arity = ar_map;
              this_module = thism;
              var_ctr = 0;
              scope = []
            } in
  let (_arity, ctx, ast) = push_lams ctx ast in
  let ctx = {ctx with top_arity = ar_map} in
  { name = Atom (Id.to_string name);
    args = List.map (fun i -> i.evar) (List.rev ctx.scope);
    ast = ml_to_erlang table ctx ast
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
    panic (str "Extraction: provided filename is not a valid identifier")

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
