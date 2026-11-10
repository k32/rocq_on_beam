Declare ML Module "rocq-core-erlang".

Fixpoint add (n m : nat) {struct m} : nat :=
  match m with
  | O => n
  | S p => add (S n) p
  end.

Compute add 2 3.

(* to stdout *)
CoreErlang Extraction add.

(* to a file *)
CoreErlang Extraction "adder" add.
