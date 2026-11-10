(*-----------------------------------------------------------------------*)
(*  Core Erlang abstract syntax tree.                                    *)
(*                                                                       *)
(*  A minimal subset of Core Erlang, exactly covering what the           *)
(*  translation from MiniML (see cetrans.ml) produces.                   *)
(*----------------------------------------------------------------------- *)

(* Atoms are the names appearing in Core Erlang source: module names,
   function names, constructor tags, ... The type is isomorphic to
   [string], but it makes atom-producing sites explicit; names derived
   from Rocq should be built via [Cetrans.global_name] (in the layer
   above). Quoting and escaping happen at printing time (see
   [Ceprint.unatom]). *)
type atom = Atom of string

(* Variables: binder names in the generated code, identified by a
   unique integer (from a per-function counter, see [Cetrans.fresh_var]).
   The concrete name is chosen at printing time ([Ceprint.unvar]). *)
type var = Var of int

type expr =
  | EVar of var (* a variable *)
  | EFunRef of atom * int (* 'f'/n : a function as a value *)
  | EInt of string (* integer literal (decimal rep) *)
  | EFloat of string (* float literal (decimal rep) *)
  | EString of string (* string literal (unescaped) *)
  | EAtom of atom (* 'atom' *)
  | ENil (* [] : the empty list *)
  | ECons of expr * expr (* [H|T] : list cons *)
  | ETuple of expr list (* {e1,...,en} : untagged tuple *)
  | EMap of (expr * expr) list (* ~{k=>v,...}~ : map literal *)
  | EBinary of string (* #<...># binary of raw bytes *)
  | EApply of callee * expr list
    (* call 'mod':'fun' (args) / apply H (args) / primop 'p' (args) *)
  | EFun of var list * expr (* fun (x1,...,xn) -> body *)
  | ELet of var * expr * expr (* let x = e1 in e2 *)
  | ELetrec of letrec_binding list * expr (* letrec b1 ... bn in body *)
  | ECase of expr * (pattern * expr * expr) list
    (* case e of <p> when g -> e ... end;
       the guard is usually 'true' *)

(* what is being applied: a (remote) global function, an expression
   (variable, fun reference, ...), or a primop *)
and callee =
  | CGlobal of atom * atom (* 'mod':'fun' *)
  | CExpr of expr (* any apply-able expression *)
  | CPrimop of atom (* primop 'name' *)

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
  | PInt of string (* integer literal, e.g. 0 *)
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

(* A whole Core Erlang module *)
type module_ = { mod_name : atom; mod_funcs : fundef list }
