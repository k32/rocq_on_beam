Declare ML Module "coreerlang_plugin".

Require Import List.
Import ListNotations.

(* Currying: n-ary Rocq functions are extracted as uncurried Core
   Erlang functions, so partial applications must be eta-expanded. *)

Definition add3 (a b c : nat) : nat := a + b + c.

(* a toplevel constant that is a partial application (1 arg missing) *)
Definition add12 : nat -> nat := add3 1 2.

(* a higher-order function built by double partial application *)
Definition compose (A B C : Type) (f : B -> C) (g : A -> B) (x : A) : C :=
  f (g x).

Definition succ_then_add2 : nat -> nat :=
  compose nat nat nat (add3 0 2) (fun n => S n).

(* partial application passed to a higher-order function *)
Definition map_add2 (l : list nat) : list nat :=
  map (add3 0 2) l.

(* fully curried application of an anonymous lambda *)
Definition apply_both (f : nat -> nat) (g : nat -> nat) (n : nat) : nat :=
  f (g n).

Definition twice_curried : nat -> nat :=
  apply_both (fun n => n + n) (fun n => S n).

CoreErlang Extraction "test_core/curry" add12 export succ_then_add2 export map_add2 export twice_curried export.
