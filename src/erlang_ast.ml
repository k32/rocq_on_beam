open Extraction_plugin
open Names
open Libnames
open Pp

type atom = Atom of string

type var = Var of int

type e_local = atom * int

type e_global = atom * atom * int

type e_literal =
  | EInt of Z.t
  | EFloat of Float64.t
  | EBinary of bytes
  | EFunRefL of e_local
  | EFunRefG of e_global
  | ENil

type e_constr =
  | ELit of e_literal
  | EVar of var
  | ECons of e_constr * e_constr
  | ETuple of e_constr list
  | EMap of (e_constr * e_constr) list

and e_pattern =
  | EWildcard
  | ELit of e_literal
  | EVar of var
  | ECons of e_pattern * e_pattern
  | ETuple of e_pattern list
  | EMap of (e_pattern * e_pattern) list

and e_callee =
  | ECallL of e_local
  | ECallG of e_global
  | ECallV of var

and e_ast =
  | EConstr of e_constr
  | EApply of e_callee * e_ast list
  | ELet of var * e_ast * e_ast
  | ECase of e_ast * (e_pattern * e_ast) list
  | EFun of var list * e_ast
  | EFix of var * var list * e_ast

type e_module = {
    exports : e_local list;
    locals : (e_local * e_ast) list
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

let pr_var = function
  | Var v -> str "_" ++ int v

let rec pr_constr = function
  | ELit l -> pr_literal l
  | EVar v -> pr_var v
  | ECons (hd, tl) ->
     hv 1 (str "[" ++ pr_constr hd ++ pr_spcbar () ++ pr_constr tl ++ str "]")
  | ETuple l ->
     hv 1 (str "{" ++ prlist_with_sep pr_comma pr_constr l ++ str "}")
  | EMap l ->
     let pr_assoc (k, v) = pr_constr k ++ str " => " ++ pr_constr v in
     hv 2 (str "#{" ++ prlist_with_sep pr_comma pr_assoc l ++ str "}")

let rec pr_pattern = function
  | EWildcard -> str "_"
  | ELit l -> pr_literal l
  | EVar v -> pr_var v
  | ECons (hd, tl) ->
     str "[" ++ pr_pattern hd ++ pr_spcbar () ++ pr_pattern tl ++ str "]"
  | ETuple l ->
     hv 1 (str "{" ++ prlist_with_sep pr_comma pr_pattern l ++ str "}")
  | EMap l ->
     let pr_assoc (k, v) = pr_pattern k ++ str " := " ++ pr_pattern v in
     hv 2 (str "#{" ++ prlist_with_sep pr_comma pr_assoc l ++ str "}")

let pr_callee = function
  | ECallL (f, _) -> pr_atom f
  | ECallG (m, f, _) -> h (pr_atom m ++ str ":" ++ pr_atom f)
  | ECallV v -> pr_var v

let pr_block preamble body =
  h (v 2 (preamble ++ brk (0, 0) ++ body) ++
       fnl () ++ str "end" ++ brk (1, 0))

let rec pr_ast = function
  | EConstr c -> pr_constr c
  | EApply (callee, args) ->
     pr_callee callee ++ surround (prlist_with_sep pr_comma pr_ast args)
  | ELet (v, a, rest) ->
     pr_var v ++ str " = " ++ h (pr_ast a) ++ str "," ++ fnl () ++
       pr_ast rest
  | ECase (scrutinee, branches) ->
     let preamble = str "case " ++ pr_ast scrutinee ++ str " of" in
     let br_sep () = str ";" ++ fnl () in
     let pr_branch (pat, ast) = v 2 (pr_pattern pat ++ str " ->" ++ brk (0, 0) ++ pr_ast ast) in
     pr_block preamble (prlist_with_sep br_sep pr_branch branches)
  | EFun (args, body) ->
     let preamble = str "fun" ++ surround (prlist_with_sep pr_comma pr_var args) ++ str " ->" in
     pr_block preamble (pr_ast body)
  | EFix (name, args, body) ->
     let preamble = str "fun " ++ pr_var name ++ surround (prlist_with_sep pr_comma pr_var args) ++ str " ->" in
     pr_block preamble (pr_ast body)

(* testing - remove *)

let test_nil = EConstr (ELit ENil)

let test_bin str = ELit (EBinary (String.to_bytes str))

let test_binp str : e_pattern = ELit (EBinary (String.to_bytes str))

let ast1 a = ELet ((Var 1000),
                   (EConstr (ETuple [ECons (test_bin "aaaa", (ELit ENil));
                                     EVar (Var 2);
                                     ELit (EFunRefG (Atom "Foo.bar", Atom "handle_call", 3));
                                     ELit (EFunRefL (Atom "handle'_call", 3));
                                     ELit (EBinary (String.to_bytes "aafadsfasdf"));
                                     ELit (EFloat (Float64.of_float 0.1))]
                   )), a)


let test_case b =
  ECase (EConstr (EVar (Var 1)),
         [(test_binp "", ast1 b);
          (EMap [test_binp "bar", EWildcard], test_nil);
          (EWildcard, ast1 b)])

let test_fun b = EFun ([Var 2; Var 1], test_case b)

let test x = print_endline (string_of_ppcmds (pr_ast x))
