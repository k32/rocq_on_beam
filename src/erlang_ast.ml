open Extraction_plugin
open Names
open Libnames
open Pp

type atom = Atom of string

type var = Var of string * int

type e_local = atom * int

type e_global = atom * atom * int

type e_literal =
  | EInt of Z.t
  | EFloat of Float64.t
  | EBinary of bytes
  | EFunRefL of e_local
  | EFunRefG of e_global
  | EAtom of atom
  | ENil

and e_pattern =
  | EPWildcard
  | EPLit of e_literal
  | EPVar of var
  | EPCons of e_pattern * e_pattern
  | EPTuple of e_pattern list
  | EPMap of (e_pattern * e_pattern) list

and e_callee =
  | ECallL of e_local
  | ECallG of e_global
  | ECallV of var

and e_ast =
  | ELit of e_literal
  | EVar of var
  | ECons of e_ast * e_ast
  | ETuple of e_ast list
  | EMap of (e_ast * e_ast) list
  | EApply of e_callee * e_ast list
  | ELet of var * e_ast * e_ast
  | ECase of e_ast * (e_pattern * e_ast) list
  | EFun of var list * e_ast
  | EFix of var * var list * e_ast

type e_defn = {
    name : atom;
    args : var list;
    ast : e_ast;
}

type e_module = {
    name : atom;
    exports : e_local list;
    locals : e_defn array
}

let escape_atom s =
  let buf = Buffer.create (String.length s + 5) in
  String.iter
    (fun c ->
      if c = '\'' then Buffer.add_string buf "\\'" else Buffer.add_char buf c)
    s;
  Buffer.contents buf

let pr_atom = function
  | Atom s -> str "'" ++ str (escape_atom s) ++ str "'"

let pr_literal = function
  | EInt i -> str (Z.to_string i)
  | EFloat f -> str (Float64.to_string f)
  | EBinary b ->
     let chars = Array.map Char.code (Array.of_seq (Bytes.to_seq b)) in
     h (str "<<" ++ prvect_with_sep pr_comma int chars ++ str ">>")
  | EFunRefL (f, a) ->
     h (str "fun " ++ pr_atom f ++ str "/" ++ int a)
  | EFunRefG (m, f, a) ->
     h (str "fun " ++ pr_atom m ++ str ":" ++ pr_atom f ++ str "/" ++ int a)
  | ENil ->
     str "[]"
  | EAtom a ->
     pr_atom a

let pr_var = function
  | Var (name, v) -> h (str "_" ++ str name ++ str "@" ++ int v)

let rec pr_pattern = function
  | EPWildcard -> str "_"
  | EPLit l -> pr_literal l
  | EPVar v -> pr_var v
  | EPCons (hd, tl) ->
     str "[" ++ pr_pattern hd ++ pr_spcbar () ++ pr_pattern tl ++ str "]"
  | EPTuple l ->
     hv 1 (str "{" ++ prlist_with_sep pr_comma pr_pattern l ++ str "}")
  | EPMap l ->
     let pr_assoc (k, v) = pr_pattern k ++ str " := " ++ pr_pattern v in
     hv 2 (str "#{" ++ prlist_with_sep pr_comma pr_assoc l ++ str "}")

let pr_callee = function
  | ECallL (f, _) -> pr_atom f
  | ECallG (m, f, _) -> h (pr_atom m ++ str ":" ++ pr_atom f)
  | ECallV v -> pr_var v

let pr_block preamble body =
  h (v 2 (preamble ++ cut () ++ body) ++
       fnl () ++ str "end" ++ cut ())

let rec pr_ast = function
  | ELit l -> pr_literal l
  | EVar v -> pr_var v
  | ECons (hd, tl) ->
     hv 1 (str "[" ++ pr_ast hd ++ pr_spcbar () ++ pr_ast tl ++ str "]")
  | ETuple l ->
     hv 1 (str "{" ++ prlist_with_sep pr_comma pr_ast l ++ str "}")
  | EMap l ->
     let pr_assoc (k, v) = pr_ast k ++ str " => " ++ pr_ast v in
     hv 2 (str "#{" ++ prlist_with_sep pr_comma pr_assoc l ++ str "}")
  | EApply (callee, args) ->
     pr_callee callee ++ surround (prlist_with_sep pr_comma pr_ast args)
  | ELet (v, a, rest) ->
     pr_var v ++ str " = " ++ h (pr_ast a) ++ str "," ++ fnl () ++
       pr_ast rest
  | ECase (scrutinee, branches) ->
     let preamble = str "case " ++ pr_ast scrutinee ++ str " of" in
     let br_sep () = str ";" ++ fnl () in
     let pr_branch (pat, ast) = v 2 (pr_pattern pat ++ str " ->" ++ cut () ++ pr_ast ast) in
     pr_block preamble (prlist_with_sep br_sep pr_branch branches)
  | EFun (args, body) ->
     let preamble = str "fun" ++ surround (prlist_with_sep pr_comma pr_var args) ++ str " ->" in
     pr_block preamble (pr_ast body)
  | EFix (name, args, body) ->
     let preamble = str "fun " ++ pr_var name ++ surround (prlist_with_sep pr_comma pr_var args) ++ str " ->" in
     pr_block preamble (pr_ast body)

let pr_toplevel = function
  | {name; args; ast} ->
     let args = prlist_with_sep pr_comma pr_var args in
     let decl = h (pr_atom name ++ surround args ++ str " ->") in
     v 2 (decl ++ cut () ++ pr_ast ast ++ str ".")

let pr_toplevels fns =
  let sep _ = brk (0, -1000) ++ cut () in
  v 0 (prvect_with_sep sep pr_toplevel fns)

let pr_mod_preamble m =
  h (str "-module" ++ surround (pr_atom m) ++ str ".") ++ fnl ()

let pr_exports e =
  let pr_export (f, a) = h (pr_atom f ++ str "/" ++ int a) in
  let el = prlist_with_sep pr_comma pr_export e in
  let e = str "-export([" ++ hv 0 (el ++ cut () ++ str "]).") in
  v 0 (e ++ brk (0, -1000) ++ cut ())

let pr_module = function
  | {name = m; exports = e; locals = fns} ->
     pr_mod_preamble m ++
       pr_exports e ++
       pr_toplevels fns
