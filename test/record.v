Declare ML Module "rocq-core-erlang".

(* A Rocq Record is an inductive with one constructor; extraction turns
   the projections into functions that match on the constructor. *)

Record point (A : Type) := mkPoint {
  px : A;
  py : A
}.

Definition origin : point nat := mkPoint nat O O.

Definition sum_coords (op : nat -> nat -> nat) (p : point nat) : nat :=
  op (px _ p) (py _ p).

Definition translate (p : point nat) (dx dy : nat) : point nat :=
  match p with
  | mkPoint _ x y => mkPoint nat (x + dx) (y + dy)
  end.

(* a record whose field type is a function *)
Record wrapper (A B : Type) := mkWrapper { unwrap : A -> B }.

Definition apply_wrapped (w : wrapper nat nat) (n : nat) : nat :=
  unwrap _ _ w n.

Definition adder_wrapper : wrapper nat nat :=
  mkWrapper nat nat (fun n => n + n).

CoreErlang Separate Extraction origin sum_coords translate apply_wrapped adder_wrapper Nat.add.
