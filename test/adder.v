Declare ML Module "coreerlang_plugin".

Fixpoint add (n m : nat) : nat :=
  match n with
  | O => m
  | S p => S (add p m)
  end.

Compute add 2 3.

(* to stdout *)
CoreErlang Extraction add.

(* to a file *)
CoreErlang Extraction "test_core/adder" add export.
