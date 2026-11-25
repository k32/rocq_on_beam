Declare ML Module "rocq_on_beam".
Require Extraction.
Extraction Language Erlang.

From Stdlib Require Import List.
Import ListNotations.

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
