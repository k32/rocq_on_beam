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
