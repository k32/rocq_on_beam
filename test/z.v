From Stdlib Require Import ZArith.

Declare ML Module "rocq-core-erlang".

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
Definition eq (x y : Z) : bool := x =? y.

(* unary library operations: unary ML functions rendered as binary BIF
   calls with a synthetic operand (opp -> 0 - X, succ -> X + 1, ...) *)
Definition negx (x : Z) : Z := - x.
Definition succx (x : Z) : Z := Z.succ x.
Definition predx (x : Z) : Z := Z.pred x.

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

CoreErlang Extraction "z" fourtytwo minus7 big
  sum prod mx absdiff eq
  negx succx predx
  half_away ofn ton sgn.
