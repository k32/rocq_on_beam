Declare ML Module "rocq-core-erlang".

From Stdlib Require Import List ZArith Arith.
Require Import Erlang.Runtime.
Import ListNotations.

(* A shadowing chain: lets, lambdas and function arguments all reusing
   the same names, with closures capturing earlier bindings. *)
Definition shadows (n : nat) : nat :=
  let x := n in
  let f := fun x => x + 1 in
  let x := f x in
  let g := fun h => h x + x in
  let x := g (fun x => x * 2) in
  n + x.

Definition shadows_result : nat := Eval vm_compute in shadows 7.

(* Mutual recursion with an argument-swapping helper *)
Fixpoint meven (n : nat) : bool :=
  match n with
  | 0 => true
  | S m => modd m
  end
with modd (n : nat) : bool :=
  match n with
  | 0 => false
  | S m => meven m
  end.

Definition meven_result : bool := Eval vm_compute in meven 10.

(* Church-style exponentiation: iterated function composition,
   partially applied functions all the way down *)
Fixpoint iter (n : nat) (f : nat -> nat) (x : nat) : nat :=
  match n with
  | 0 => x
  | S m => iter m f (f x)
  end.

Definition church (b e : nat) : nat := iter e (iter b S) 0.

Definition church_result : nat := Eval vm_compute in church 2 10.

(* Deep nested patterns over option/list/pair/bool with wildcards and
   overlapping clauses *)
Definition deepmatch (v : option (list (option (nat * bool)))) : nat :=
  match v with
  | Some [Some (n, true); None; Some (_, false)] => n
  | Some [None] => 1
  | Some (Some (n, _) :: _) => n + 2
  | _ => 0
  end.

Definition deepmatch_result : nat :=
  Eval vm_compute in deepmatch (Some [Some (3, true); None; Some (9, false)]).

(* A local fix inside a branch of another local fix, with a let between *)
Definition fixnest (n : nat) : nat :=
  let outer := fix go (acc i : nat) : nat :=
                 match i with
                 | 0 => acc
                 | S j =>
                     let inner := fix dbl (k : nat) : nat :=
                                    match k with
                                    | 0 => 0
                                    | S m => S (S (dbl m))
                                    end
                     in go (inner acc + 1) j
                 end
  in outer 1 n.

Definition fixnest_result : nat := Eval vm_compute in fixnest 5.

(* Big-int arithmetic through the Z lowering *)
Fixpoint zfact (n : nat) : Z :=
  match n with
  | 0 => 1%Z
  | S m => Z.mul (Z.of_nat n) (zfact m)
  end.

Definition zfact_result : Z := Eval vm_compute in zfact 20.

(* Polymorphic self-application *)
Definition selfapp (n : nat) : nat :=
  (((fun (t : forall X : Type, X -> X) => t (forall X : Type, X -> X) t)
      (fun (X : Type) (y : X) => y))
    nat) n.

Definition selfapp_result : nat := Eval vm_compute in selfapp 41.

(* HMap, Z and native lists in one pipeline *)
Definition pipeline (n : nat) : nat :=
  let m := HMap.put 1 (Z.of_nat n) HMap.make in
  let m := HMap.put 2 (Z.of_nat (n * 2)) m in
  HMap.fold nat (fun k v acc => Z.to_nat (v + Z.of_nat k) + acc) 0 m.

Definition pipeline_result : nat := Eval vm_compute in pipeline 5.

CoreErlang Extraction "patho" shadows shadows_result meven meven_result church church_result deepmatch deepmatch_result fixnest fixnest_result zfact zfact_result selfapp selfapp_result pipeline pipeline_result.
