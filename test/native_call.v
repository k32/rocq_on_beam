Declare ML Module "rocq-to-erlang".

From Stdlib Require Import Arith.

(* User-declared native calls: any definition can be mapped onto an
   Erlang function "module:function/arity"; the trailing integer counts
   leading Rocq arguments dropped untranslated, 0 if none. *)

Definition mymul (a b : nat) : nat := a * b.
CoreErlang Native Call mymul := "erlang:*/2" 0.

(* the first argument is dropped untranslated (think typeclass
   dictionary); the remaining one goes to a unary Erlang function *)
Definition dropfirst (a b : nat) : nat := b.
CoreErlang Native Call dropfirst := "erlang:abs/1" 1.

Definition twelve : nat := mymul 3 4.
Definition five : nat := dropfirst 99 5.

(* exported wrappers so the suite can call the native targets with
   runtime arguments *)
Definition m (a b : nat) : nat := mymul a b.
Definition d (a b : nat) : nat := dropfirst a b.

CoreErlang Extraction "native_call" twelve five m d.
