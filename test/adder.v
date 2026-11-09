Declare ML Module "coreerlang_plugin".

(* tail-recursive: the accumulator is the first argument, recursion on
   the second (and the recursive call is in tail position) *)
Fixpoint add (n m : nat) {struct m} : nat :=
  match m with
  | O => n
  | S p => add (S n) p
  end.

Compute add 2 3.

(* to stdout *)
CoreErlang Extraction add.

(* to a file *)
CoreErlang Extraction "test_core/adder" export(add).
