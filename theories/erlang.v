(** * Placeholder Definitions of Erlang BIFs
 *)

Require Extraction.
Declare ML Module "rocq_on_beam".

Extraction Language Erlang.

(** [Env] is a type of dummy "native environment" argument accepted by
some functions defined in this module.

It allows us to define placeholders for effectful BIFs and NIFs, that
have no equivalent in Rocq without resorting to the axioms. Such
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

Extract Constant make_ref => "erlang:make_ref".

Definition yield {env : Env} : unit :=
  match env with
  end.

Extract Constant yield => "erlang:yield".
