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
   above). The rocq_ prefix, quoting and escaping happen at printing
   time (see [Ceprint.unatom]). *)
type atom = Atom of string

(* Variables: binder names in the generated code. Also isomorphic to
   [string]; created by [Cetrans.fresh_var], which sanitizes the Rocq
   identifier and ensures uniqueness within the enclosing function. *)
type var = Var of string

type expr =
  | EVar of var                      (* a variable *)
  | EFunRef of atom * int             (* 'f'/n : a function as a value *)
  | EInt of string                    (* integer literal (decimal rep) *)
  | EFloat of string                  (* float literal (decimal rep) *)
  | EString of string                 (* string literal (unescaped) *)
  | EAtom of atom                     (* 'atom' *)
  | ETuple of expr list               (* {e1,...,en} : untagged tuple *)
  | ECall of atom * atom * expr list
                                      (* call 'mod':'fun' (args) *)
  | EApply of expr * expr list        (* apply H (args) *)
  | EFun of var list * expr           (* fun (x1,...,xn) -> body *)
  | ELet of var * expr * expr         (* let x = e1 in e2 *)
  | ELetrec of letrec_binding list * expr
                                      (* letrec b1 ... bn in body *)
  | ECase of expr * (pattern * expr) list
                                      (* case e of <p> when 'true' -> e ... end *)

and letrec_binding = {
  lr_name : atom;                     (* function name, e.g. rocq_fix_1_0 *)
  lr_params : var list;               (* lambda parameters, outermost-first *)
  lr_body : expr;
}

and pattern =
  | PWild                             (* _ *)
  | PVar of var                       (* a variable *)
  | PAtom of atom                     (* 'tag' *)
  | PTuple of pattern list            (* {p1,...,pn} *)

(* One toplevel function definition 'name'/n = fun (params) -> body.
   [fun_exported] controls whether it is listed in the module exports;
   non-exported functions are still defined (and callable from within
   the module) but not visible from outside. *)
type fundef = {
  fun_name : atom;
  fun_params : var list;              (* outermost-first *)
  fun_body : expr;
  fun_exported : bool;
}

(* A whole Core Erlang module *)
type module_ = {
  mod_name : atom;
  mod_funcs : fundef list;
}
