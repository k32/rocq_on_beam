From Stdlib Require Import ZArith.

Declare ML Module "coreerlang_plugin".

Open Scope Z_scope.

(* literals fold to integers (binary representation interpreted) *)
Definition fourtytwo : Z := 42.
Definition minus7 : Z := -7.
Definition big : Z := 123456789012345678901234567890.

(* library operations map wholesale to Erlang BIFs *)
Definition sum (x y : Z) : Z := x + y.
Definition prod (x y : Z) : Z := x * y.
Definition mx (x y : Z) : Z := Z.max x y.
Definition absdiff (x y : Z) : Z := Z.abs (x - y).
Definition cmp (x y : Z) : comparison := Z.compare x y.
Definition eq (x y : Z) : bool := x =? y.

(* partially applied library function *)
Definition half_away (x : Z) : Z := Z.sub x 1.

(* conversions: both sides are integers now, so they are the identity *)
Definition ofn (n : nat) : Z := Z.of_nat n.
Definition ton (z : Z) : nat := Z.to_nat z.

(* a user match on the Z structure *)
Definition sgn (z : Z) : Z :=
  match z with
  | Z0 => 0
  | Zpos _ => 1
  | Zneg _ => -1
  end.

CoreErlang Extraction "test_core/z" export(fourtytwo) export(minus7) export(big)
  export(sum) export(prod) export(mx) export(absdiff) export(cmp) export(eq)
  export(half_away) export(ofn) export(ton) export(sgn).
