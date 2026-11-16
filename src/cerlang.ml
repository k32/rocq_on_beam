(*-----------------------------------------------------------------------
  Erlang abstract syntax tree.

  A minimal subset of Erlang, exactly covering what the
  translation from MiniML (see cetrans.ml) produces.
-----------------------------------------------------------------------*)

type atom = Atom of string

(* Variables: binder names in the generated code, identified by a
   unique integer (from a per-function counter, see [Cetrans.fresh_var]).
   The concrete name is chosen at printing time ([Ceprint.unvar]). *)
type var = Var of int

type expr =
  | EVar of var (* a variable *)
  | EFunRef of atom * int (* 'f'/n : a function as a value *)
  | EInt of Z.t (* integer literal (arbitrary precision) *)
  | EFloat of float (* float literal (64-bit value) *)
  | EString of string (* string literal (unescaped) *)
  | EAtom of atom (* 'atom' *)
  | ENil (* [] : the empty list *)
  | ECons of expr * expr (* [H|T] : list cons *)
  | ETuple of expr list (* {e1,...,en} : untagged tuple *)
  | EMap of (expr * expr) list (* #{k=>v,...} : map literal *)
  | EBinary of string
  | EApply of callee * expr list (* apply arguments to something *)
  | EFun of var list * expr (* fun (x1,...,xn) -> body *)
  | ELet of var * expr * expr (* x = e1; *)
  | ELetrec of letrec_binding list * expr (* Local recursive function *)
  | ECase of expr * (pattern * expr * expr) list

(* what is being applied: a (remote) global function, an expression
   (variable, fun reference, ...) *)
and callee =
  | CGlobal of atom * atom (* 'mod':'fun' *)
  | CExpr of expr (* any apply-able expression *)

and letrec_binding = {
  lr_name : atom; (* function name, e.g. fix_1_0 *)
  lr_params : var list; (* lambda parameters, outermost-first *)
  lr_body : expr;
}

and pattern =
  | PWild (* _ *)
  | PVar of var (* a variable *)
  | PAtom of atom (* 'tag' *)
  | PNil (* [] *)
  | PCons of pattern * pattern (* [P|P] *)
  | PInt of Z.t (* integer literal, e.g. 0 *)
  | PTuple of pattern list (* {p1,...,pn} *)

(* One toplevel function definition 'name'/n = fun (params) -> body.
   [fun_exported] controls whether it is listed in the module exports;
   non-exported functions are still defined (and callable from within
   the module) but not visible from outside. *)
type fundef = {
  fun_name : atom;
  fun_params : var list; (* outermost-first *)
  fun_body : expr;
  fun_exported : bool;
}

(* A whole Erlang module *)
type module_ = { mod_name : atom; mod_funcs : fundef list }
