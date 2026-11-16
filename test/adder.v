Declare ML Module "rocq-to-erlang".

Fixpoint add (n m : nat) {struct m} : nat :=
  match m with
  | O => n
  | S p => add (S n) p
  end.

Compute add 2 3.

(* to stdout *)
Erlang Extraction add.

(* to a file *)
Erlang Separate Extraction add.
