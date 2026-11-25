Declare ML Module "rocq_on_beam".
Require Extraction.
Extraction Language Erlang.

From Stdlib Require Import List.
Import ListNotations.

Definition t_nat :=
  2.

Extraction t_nat.

Definition t_list :=
  [0; 0].

Extraction t_list.

Definition t_local_lam (a : nat) :=
  let f a := S a in
  f (f a).

Extraction t_local_lam.

(* Verify that all variables are unique inside a definition *)
Definition t_all_var_suff_unique (a : nat) :=
  let a := match a with
           | O => None
           | S a =>
               match a with
               | S a => None
               | a => Some a
               end
           end in
  let f a := match a with
             | None => None
             | Some a => Some (S a)
             end in
  f (f a).

Extraction t_all_var_suff_unique.

Fixpoint t_mutual_fix (a : nat) :=
  let x := S a in
  match a with
  | O => x
  | S a => t_mutual_fix2 a
  end
with
t_mutual_fix2 a :=
  match a with
  | O => O
  | S a => t_mutual_fix a
  end.

Extraction t_mutual_fix.

Definition t_apply_var {T : Set} (f : T -> T) (x : T) :=
  f (f x).

Extraction t_apply_var.
