(* Tests related to arity checks *)
Declare ML Module "rocq_on_beam".
Require Extraction.
Extraction Language Erlang.

From Stdlib Require Import List.
Import ListNotations.

(* Lambdas defined locally should pass the checks *)
Definition t_local_lam1 (a : nat) :=
  let f a := S a in
  f (f a).

Extraction t_local_lam1.
Definition t_local_lam2 (a : nat) :=
  let f {T1 T2} (a : T1) (b : T2) := (a, b) in
  f (f a a) a.

Extraction t_local_lam2.

(* Applying values to function arguments is allowed *)
Definition t_apply_var1 {T : Set} (f : T -> T) (x : T) :=
  f (f x).

Extraction t_apply_var1.

Definition t_apply_var2 (f : nat -> nat -> nat) x y :=
  f (f x y) x.

Extraction t_apply_var2.

(* With simple extraction, arity if [plus] is unknown. Should fail *)
Definition t_unknown_gl a b :=
  a + b.

(* Fail Extraction t_unknown_gl. *)

Definition t_local_eta {T} (a : nat) : T -> _ :=
  let fn {A B} (x : A) (y : B) := (x, y) in
  fn (fn a a).

Extraction t_local_eta.
