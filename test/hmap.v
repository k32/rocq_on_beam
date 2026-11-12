Declare ML Module "rocq-core-erlang".

From Stdlib Require Import Arith List.
Require Import Erlang.Runtime.
Import ListNotations.

(* HMap through the wrapper: extraction must render maps as
   {'HMapW', List}, distinct from regular native lists. find returns
   MapFindResult, lowered to native {ok, V} | error. *)

Definition m0 : @HMap.t nat nat := HMap.make.

Definition m1 : HMap.t := HMap.put 2 20 (HMap.put 1 10 m0).

Definition lookup (k : nat) : MapFindResult := HMap.find k m1.

Definition add (k v : nat) (m : HMap.t) : HMap.t :=
  HMap.put k v m.

Definition sum : nat := HMap.fold nat (fun k v a => k * v + a) 0 m1.

(* matching on MapFindResult from Rocq code *)
Definition get_or_zero (k : nat) : nat :=
  match lookup k with
  | ok v => v
  | error => 0
  end.

CoreErlang Separate Extraction m1 lookup add sum get_or_zero.
