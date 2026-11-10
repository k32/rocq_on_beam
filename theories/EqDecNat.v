From Stdlib Require Import List Classes.EquivDec Arith.
From LibTx Require Import Storage.Classes Storage.Instances.List.
Import ListNotations.

(* Our own EqDec instance for nat keys: extraction-friendly (the
   Stdlib instance's eq_dec field is Nat.eq_dec, which has no native
   lowering). The dictionary itself is dead code in the extracted
   module -- HMap calls drop it -- but it must still be translatable. *)
Fixpoint nat_eqdec (n m : nat) : {n = m} + {n <> m}.
Proof. decide equality. Defined.

Global Instance nat_EqDec : EqDec nat eq := nat_eqdec.
