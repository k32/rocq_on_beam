Declare ML Module "coreerlang_plugin".

Require Import List.

Fixpoint len (l : list nat) : nat :=
  match l with
  | nil => O
  | cons _ t => S (len t)
  end.

Definition double (n : nat) : nat := n + n.

Definition apply_to_two (f : nat -> nat) : nat := f 2.

Definition partially_applied := apply_to_two double.

(* mutual recursion *)
Fixpoint even (n : nat) : bool :=
  match n with
  | O => true
  | S m => odd m
  end
with odd (n : nat) : bool :=
  match n with
  | O => false
  | S m => even m
  end.

(* local recursion inside a definition *)
Definition fact_helper (n : nat) : nat :=
  (fix go (acc n : nat) : nat :=
     match n with
     | O => acc
     | S m => go (acc * S m) m
     end) 1 n.

Fixpoint sumlist (l : list nat) : nat :=
  match l with
  | nil => O
  | cons h t => h + sumlist t
  end.

CoreErlang Extraction "test_core/rich" len partially_applied even odd fact_helper sumlist.
