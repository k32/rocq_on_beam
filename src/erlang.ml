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

let unsup what = CErrors.user_err Pp.(str "Not supported " ++ str what)

type ctx = {
    var_ctr : int;
    (* Variables in the scope, for resolving de Brujin indices. Innermost first *)
    scope : var list;
  }

(* Iterate over nested lambdas, adding variables into the context.
   Return arity (number of nested lambdas), updated context and the body *)
let push_lams =
  let rec go arity ctx = function
    | MLlam (_, ast) ->
       (match ctx with
          {var_ctr = var; scope = scope} ->
          go (arity + 1) {var_ctr = var + 1; scope = Var var :: scope} ast)
    | ast ->
       (arity, ctx, ast) in
  go 0

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

let rec ast_to_erlang table ctx = function
  | MLcons (tp, gl, args) ->
     unsup "MLcons"
  | MLrel v ->
     unsup "MLrel"
  | MLapp (callee, arg) ->
     unsup "MLapp"
  | MLlam  (ident, ast) ->
     unsup "MLlam"
  | MLletin (ident, value, ast) ->
     unsup "MLletin"
  | MLglob global ->
     unsup "MLglob"
  | MLtuple elems ->
     unsup "MLtuple"
  | MLcase (tp, scrutinee, branches) ->
     unsup "MLcase"
  | MLfix  (i, args, asts) ->
     unsup "MLfix"
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
  | MLfloat petya ->
     unsup "MLfloat"
  | MLstring pstr ->
     unsup "MLstring"
  | MLparray (asts, ast) ->
     unsup "MLparray"

let decl_to_erlang table gl ast tp =
  let name = label_of_global gl in
  let (_arity, ctx, ast) = push_lams {var_ctr = 0; scope = []} ast in
  { name = Atom (Id.to_string name);
    args = ctx.scope;
    ast = ast_to_erlang table ctx ast
  }

let pp_top_fun table gl ast tp =
  let name = pp_global_name table Term gl in
  if is_custom gl
  then
    CErrors.user_err Pp.(str "Cannot extract native function " ++ str name)
  else
    let east = decl_to_erlang table gl ast tp in
    v 0 (pr_toplevel east ++ brk (0, -1000))

let pp_decl table = function
  | Dind _ -> CErrors.user_err Pp.(str "Extraction of inductives is not supported")
  | Dtype _ -> CErrors.user_err Pp.(str "Extraction of types is not supported")
  | Dterm (gl, ast, tp) ->
     pp_top_fun table gl ast tp
  | Dfix (gls, asts, tps) ->
     let f i gl = pp_top_fun table gl asts.(i) tps.(i) in
     let sep _ = brk (0, -1000) ++ cut () in
     prvecti_with_sep sep f gls

let file_naming state mp = file_of_modfile (State.get_table state) mp

let id_of_filename f =
  try Id.of_string f
  with CErrors.UserError _ ->
    CErrors.user_err
      Pp.(str "Extraction: provided filename is not a valid identifier")

let preamble table mod_name comment used_modules usf =
  pr_mod_preamble (Atom (Id.to_string mod_name))

let pp_struct table stct = str "FIXME STRUCT"

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
