(** * Definitions of Erlang BIFs
 *)

Require Extraction.
Declare ML Module "rocq_on_beam".

Extraction Language Erlang.

(** [Env] is a type of dummy "native environment" arguments accepted
by some functions defined in this module.

It allows to define placeholders for effectful BIFs and NIFs, that
have no equivalent in Rocq, without resorting to the axioms. Such
placeholder definitions can be typechecked, but they cannot be
accidentally used in a proof. Since [Env] is in [Prop], the env arument
gets erased during the extraction. *)
Definition Env : Prop := False.

(** Native pid() type *)
Inductive NPid : Set := npid (env : Env).

(** Native ref() type *)
Inductive NRef : Set := nref (env : Env).

Definition make_ref {env} : NRef :=
  nref env.

Definition yield {env : Env} : unit :=
  match env with
  end.

Extract Constant Nat.add => "erlang:'+'".
