Declare ML Module "coreerlang_plugin".

(* A dependently typed "command" datatype: the return type of doit is
   computed by ret_type, a type-level function. Extraction erases the
   dependency: ret_type disappears entirely and doit extracts to a
   plain match on the tagged constructors. *)

Inductive Arg : Type :=
  | Int : nat -> Arg
  | Bool : bool -> Arg.

Definition ret_type (a : Arg) : Set :=
  match a with
  | Int _ => nat
  | Bool _ => bool
  end.

Definition doit (a : Arg) : ret_type a :=
  match a with
  | Int n => S n
  | Bool b => negb b
  end.

Compute doit (Int 3).
Compute doit (Bool true).

CoreErlang Extraction "test_core/arg" doit.
