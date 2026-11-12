Declare ML Module "rocq-core-erlang".

(* primitive floats lower to native float literals *)
Require Import PrimFloat.

Definition e : float := 2.5.
Definition small : float := 0.125.
Definition neg : float := -1.5.

CoreErlang Separate Extraction e small neg.
