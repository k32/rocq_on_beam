Declare ML Module "coreerlang_plugin".

(* Polymorphic functions over tuples: the type parameters A and B are
   logical (in Type/Prop, erased by extraction), so the extracted code
   is untyped and works on any 2-tuple. *)

Definition myfst (A B : Type) (p : A * B) : A :=
  match p with (a, _) => a end.

Definition mysnd (A B : Type) (p : A * B) : B :=
  match p with (_, b) => b end.

Definition swap (A B : Type) (p : A * B) : B * A :=
  (mysnd A B p, myfst A B p).

(* type parameter carrying a function: still erased *)
Definition mapfst (A B : Type) (f : A -> A) (p : A * B) : A * B :=
  match p with (a, b) => (f a, b) end.

CoreErlang Extraction "test_core/poly" myfst export mysnd export swap export mapfst export.
