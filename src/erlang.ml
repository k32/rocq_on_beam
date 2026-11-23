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

let pp_atom a = str "'" ++ str (Id.to_string a) ++ str "'"
let pp_comment str = str "%% FIXME comment"

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
  str "-module" ++ surround (pp_atom mod_name) ++ str ".\n"

let pp_struct table stct = str "FIXME STRUCT"

let unsup what = CErrors.user_err Pp.(str "Not supported " ++ str what)

let eta_trans ast = ast

let cut2 () = brk (0,-100000) ++ brk (0,0)

let pp_uint i =
  str (Z.to_string (Z.of_int64 (Uint63.to_int64 i)))

let pp_float f =
  str (Float64.to_string f)

let pp_binary s =
  str "<<\"" ++ qstring (Pstring.to_string s) ++ str "\">>"

let new_env = ()

let pr_ident = function
  | Dummy -> str "_"
  | Id i -> str (Id.to_string i)
  | Tmp i -> str (Id.to_string i) ++ str "%tmp"

let rec pp_term env = function
  | MLrel i ->
     str "_" ++ int i
  | MLapp (callee, args) ->
     pp_term env callee ++
       surround (prlist_with_sep pr_comma (pp_term env) args)
  | MLlam (arg, ast) ->
     str "fun(" ++ pr_ident arg ++ str ") ->" ++ fnl () ++
       hv 0 (pp_term env ast) ++
       str "end" ++ fnl()
  | MLletin (var, assignment, ast) ->
     pr_ident var ++ str " = " ++
       pp_term env assignment ++ pr_comma () ++
       pp_term env ast
  | MLglob _ ->
     str "(MLGLOB)"
  | MLcase (tp, scrutinee, branches) ->
     let pp_sep _ = str ";" ++ fnl () in
     let pp_branch _ =  str "branch" in
     str "case " ++ pp_term env scrutinee ++ str " of" ++ fnl () ++
       prvect_with_sep pp_sep pp_branch branches ++
       fnl () ++ str "end" ++ fnl ()
  | MLuint i ->
     pp_uint i
  | MLfloat f ->
     pp_float f
  | MLstring s ->
     pp_binary s
  | _ ->
     str "(NOT SUPPORTED)"

let pp_top_fun table gl ast tp =
  let _ = eta_trans ast in
  let name = pp_global_name table Term gl in
  let def = if is_custom gl
            then CErrors.user_err Pp.(str "Cannot extract native function " ++ str name)
            else pp_term new_env ast in
  hov 0 (str name ++ str "(" ++ def) ++ cut2 ()

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
    keywords;
    file_suffix = ".erl";
    file_naming;
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
    id_of_filename;
  }

let () = register_language (External "Erlang") erlang_descr
