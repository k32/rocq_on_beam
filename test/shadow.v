Declare ML Module "coreerlang_plugin".

(* Repeated shadowing of the same Gallina name by let-binders, lambda
   binders and fix parameters, including inside nested functions. *)

Definition shadowlet (x : nat) : nat :=
  let x := x + 1 in                        (* let shadows the parameter *)
  let f := fun x => x * 2 in               (* lambda shadows the let *)
  let x := f x in
  let g := fun h => h x + x in             (* g's body sees the outer x *)
  let x := g (fun x => x + 10) in          (* argument lambda shadows x again *)
  x.

(* let-binders shadowing a fix parameter, inside the fix body *)
Definition shadowfix (x : nat) : nat :=
  let x := x * 2 in
  (fix go (x i : nat) : nat :=
     match i with
     | O => x
     | S j => let x := x + 1 in go x j
     end) x 3.

Compute shadowlet 3.
Compute shadowfix 5.

CoreErlang Extraction "test_core/shadow" export(shadowlet) export(shadowfix).
