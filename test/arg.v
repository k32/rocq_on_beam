Declare ML Module "coreerlang_plugin".

From Stdlib Require Import ZArith.
Open Scope Z_scope.

(* A dependently typed "command" datatype: the return type of doit is
   computed by ret_type, a type-level function. Extraction erases the
   dependency: ret_type disappears entirely and doit extracts to a
   plain match on the tagged constructors. *)

Inductive Arg : Type :=
  | Int : Z -> Arg
  | Bool : bool -> Arg.

Definition ret_type (a : Arg) : Set :=
  match a with
  | Int _ => Z
  | Bool _ => bool
  end.

Definition doit (a : Arg) : ret_type a :=
  match a with
  | Int n => n + 1
  | Bool b => negb b
  end.

Compute doit (Int 3).
Compute doit (Int (-123456789012345678901234567890)).
Compute doit (Bool true).

CoreErlang Extraction "test_core/arg" doit.
