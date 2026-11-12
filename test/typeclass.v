From Stdlib Require Import ZArith.

Declare ML Module "rocq-core-erlang".

(* A typeclass: just a one-constructor record with parameters *)
Class Show A := {
  show : A -> nat;
  shown : A -> nat
}.

(* An instance: a nullary constant of record type *)
Instance show_Z : Show Z := {|
  show := fun z => 0;
  shown := fun z => Z.to_nat (Z.abs z)
|}.

Instance show_nat : Show nat := {|
  show := fun n => 1;
  shown := fun n => n
|}.

(* Method use: the dictionary argument is resolved at elaboration *)
Definition describe (z : Z) : nat := show z.
Definition both (z : Z) (n : nat) : nat := shown z + shown n.

(* Superclass: nested dictionaries (a record field that is a record) *)
Class DoubleShow A := {
  DS_show :> Show A;
  DS_two : A -> nat
}.

Instance ds_Z : DoubleShow Z := {|
  DS_show := show_Z;
  DS_two := fun z => 2
|}.

Definition describe2 (z : Z) : nat := DS_two z.

(* Generic function over the class: dictionary becomes an explicit arg *)
Definition showTwice {A} (s : Show A) (x : A) : nat :=
  show x + show x.

CoreErlang Extraction "typeclass" describe both describe2 showTwice.
