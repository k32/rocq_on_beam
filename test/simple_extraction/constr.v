(* Test extraction of constructors *)
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
