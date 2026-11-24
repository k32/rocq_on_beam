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

let preamble table mod_name comment used_modules usf =
  pr_mod_preamble (Atom (Id.to_string mod_name))

let pp_struct table stct = str "FIXME STRUCT"

let unsup what = CErrors.user_err Pp.(str "Not supported " ++ str what)

let to_erlang table gl _ast tp =
  let body = test_fun (test_fun (ast1 (ast1 test_nil))) in
  {name = Atom "foo"; args = [Var 1; Var 2; Var 3]; ast = body}

let pp_top_fun table gl ast tp =
  let name = pp_global_name table Term gl in
  if is_custom gl
  then
    CErrors.user_err Pp.(str "Cannot extract native function " ++ str name)
  else
    let east = to_erlang table gl ast tp in
    pr_toplevel east

let pp_decl table = function
  | Dind _ -> CErrors.user_err Pp.(str "Extraction of inductives is not supported")
  | Dtype _ -> CErrors.user_err Pp.(str "Extraction of types is not supported")
  | Dterm (gl, ast, tp) ->
     pp_top_fun table gl ast tp
  | Dfix (gls, asts, tps) ->
     let f i gl =
       pp_top_fun table gl asts.(i) tps.(i) in
     prvecti f gls

let file_naming state mp = file_of_modfile (State.get_table state) mp

let id_of_filename f =
  try Id.of_string f
  with CErrors.UserError _ ->
    CErrors.user_err
      Pp.(str "Extraction: provided filename is not a valid identifier")

let erlang_descr =
  {
    keywords = keywords;
    file_suffix = ".erl";
    file_naming = file_naming;
    preamble = preamble;
    pp_struct = pp_struct;
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
