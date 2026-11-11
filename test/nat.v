Declare ML Module "rocq-core-erlang".

(* nat lowers to native integers: constructor literals fold (S-chains
   become one integer), matches become guarded clauses, and the values
   interoperate directly with Erlang arithmetic. *)

Definition big : nat := 512.

Definition predn (n : nat) : nat :=
  match n with
  | O => O
  | S m => m
  end.

Definition three : nat := S (S (S O)).

CoreErlang Extraction "nat" big predn three.
