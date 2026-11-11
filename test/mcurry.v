Declare ML Module "rocq-core-erlang".

From Stdlib Require Import List ZArith Arith.
Require Import Erlang.Runtime.
Import ListNotations.

(* Curried uses of HMap methods: fold/find/remove partially applied and
   passed around as first-class functions. Oracles are kernel-computed
   (vm_compute). *)

Definition m1 : HMap.t := HMap.put 1 10 (HMap.put 2 20 HMap.make).
Definition m2 : HMap.t := HMap.put 1 111 (HMap.put 3 30 HMap.make).

(* fold curried: f and acc fixed, the map-accepting function is passed
   to another function *)
Definition sumof : (@HMap.t nat nat -> nat) -> @HMap.t nat nat -> nat :=
  fun g m => g m.

Definition cfold (m : HMap.t) : nat :=
  sumof (HMap.fold nat (fun _ v a => v + a) 0) m.

Definition cfold_result : nat := Eval vm_compute in cfold m1.

(* find curried: a fixed-key finder mapped over a list of maps *)
Definition cfind : nat -> list (@HMap.t nat nat) -> list MapFindResult :=
  fun k l => map (HMap.find k) l.

Definition cfind_result : list MapFindResult :=
  Eval vm_compute in cfind 1 [m1; HMap.make; m2].

(* remove curried: applied twice through an identity-style combinator *)
Definition twice : (@HMap.t nat nat -> @HMap.t nat nat)
                   -> @HMap.t nat nat -> @HMap.t nat nat :=
  fun f m => f (f m).

Definition cremove (m : HMap.t) : HMap.t :=
  twice (HMap.remove 1) m.

Definition cremove_result : list MapFindResult :=
  Eval vm_compute in map (HMap.find 1) [cremove m1; cremove m2].

CoreErlang Extraction "mcurry" m1 m2 cfold cfind cremove cfold_result cfind_result cremove_result.
