Declare ML Module "coreerlang_plugin".

(* Nested, non-recursive function definitions: let-bound lambdas
   referring to outer let-bound lambdas. *)

Definition nested (n : nat) : nat :=
  let f : nat -> nat := fun x => x + 1 in
  let g : nat -> nat := fun x => f (f x) in
  let h : nat -> nat := g in
  h (f n).

(* a lambda immediately applied to several arguments *)
Definition applied (n : nat) : nat :=
  (fun a b => a * 10 + b) n 3.

(* a let-bound recursive function (a fix nested in a let) *)
Definition deep (n : nat) : nat :=
  let pow2 := fix p (i : nat) : nat :=
    match i with
    | O => 1
    | S j => 2 * p j
    end
  in pow2 n.

(* a lambda defined inside a function body, shadowing the outer
   parameter; the inner binding must win *)
Definition shadow (n : nat) : nat :=
  (fun n => n + nested 0) (n + 1).

CoreErlang Extraction "test_core/nested" nested applied deep shadow.
