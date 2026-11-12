Declare ML Module "rocq-core-erlang".

From Erlang Require Import GenServer.
(* From Hammer Require Import Hammer Tactics. *)

(* Two different gen_servers: a counter and a LIFO. Each is a module
   ascribing to the gen_server module type; separate extraction emits
   each as its own dotted Erlang module ('gen_server.Counter',
   'gen_server.LIFO'). Each test is a separate OTP application
   running on its own node, so shared dependency modules of different
   tests never meet. *)

Module Counter <: gen_server.
  Inductive ccall := get | add (n : nat).

  Inductive ccast := inc | dec.

  Inductive cinfo := reset (n : nat).

  Definition t :=
    {| init_arg_t := nat;
       state_t := nat;
       exit_reason_t := bool;
       call_t := ccall;
       cast_t := ccast;
       info_t := cinfo;
       reply_t _ := nat;
    |}.

  Definition init (i : @init_arg_t t) : @init_res t :=
    @i_ok t i.

  Definition handle_cast (c : @cast_t t) (s : @state_t t) : @handle_cast_res t :=
    match c with
    | inc => @cst_noreply t (S s)
    | dec => @cst_noreply t (s - 1)
    end.

  Definition handle_call `{FromT} (call : @call_t t) (_ : from_t) (s : @state_t t) : @call_res t call :=
    match call with
    | get => @c_reply t call s s
    | add n => @c_reply t call n (s + n)
    end.

  Definition handle_info (i : @info_t t) (s : @state_t t) : @info_res t :=
    match i with
    | reset n => @inf_noreply t n
    end.
End Counter.

From Stdlib Require Import List.
Import ListNotations.

Module LIFO <: gen_server.
  Record State :=
    { data : list nat;
      len : nat
    }.

  Inductive Call :=
  | pop
  | get_len
  | push (n : nat).

  Inductive Cast :=
  | apush (n : nat)
  | adrop.

  Inductive Info := flush (b : bool).

  Definition reply (c : Call) : Set :=
    match c with
    | pop => option nat
    | get_len => nat
    | push _ => True
    end.

  Definition t :=
    {| init_arg_t := list nat;
       state_t := State;
       exit_reason_t := bool;
       call_t := Call;
       cast_t := Cast;
       info_t := Info;
       reply_t := reply;
    |}.

  Definition init (l : @init_arg_t t) : @init_res t :=
    @i_ok t {| data := l; len := length l |}.

  Definition handle_cast (cast : @cast_t t) (s : @state_t t) : @handle_cast_res t :=
    let (d, l) := s in
    match cast with
    | adrop =>
        match d with
        | [] =>
            @cst_noreply t s
        | _ :: d =>
            let s' := {| data := d; len := l - 1 |} in
            @cst_noreply t s'
        end
    | apush n =>
        let s' := {| data := n :: d; len := l + 1 |} in
        @cst_noreply t s'
    end.

  Definition handle_call `{FromT} (call : @call_t t) (_ : from_t) (s : @state_t t) : @call_res t call :=
    let (d, l) := s in
    match call with
    | push n =>
        let s' := {| data := n :: d; len := l + 1 |} in
        @c_reply t (push n) I s'
    | get_len =>
        @c_reply t get_len l s
    | pop =>
        match d with
        | [] =>
            @c_reply t pop None s
        | n :: d =>
            let s' := {| data := d; len := l - 1 |} in
            @c_reply t pop (Some n) s'
        end
    end.

  Definition handle_info (i : @info_t t) (s : @state_t t) : @info_res t :=
    match i with
    | flush _ =>
        let s' := {| data := []; len := 0 |} in
        @inf_noreply t s'
    end.
End LIFO.

From Stdlib Require Import Lia.

Module LIFO_state_invariant : gen_server_invariant LIFO.
  Definition prop (s : @state_t LIFO.t) :=
    let (d, l) := s in
    length d = l.

  Lemma init_valid : init_valid_t prop LIFO.init.
  Proof.
    easy.
  Qed.

  Lemma handle_cast_valid : handle_cast_valid_t prop LIFO.handle_cast.
  Proof.
    unfold prop,LIFO.handle_cast. intros cast s H.
    destruct s as [d l].
    destruct cast.
    - simpl. lia.
    - destruct d.
      + easy.
      + simpl in H. lia.
  Qed.

  Lemma handle_info_valid : handle_info_valid_t prop LIFO.handle_info.
  Proof.
    unfold prop,LIFO.handle_info.
    intros inf s H.
    now destruct inf.
  Qed.

  Lemma handle_call_valid : forall `{HF : FromT}, handle_call_valid_t prop LIFO.handle_call.
  Proof.
    unfold prop, LIFO.handle_call. intros from_t HF call from s.
    destruct s as [d l].
    destruct call.
    - destruct d.
      + easy.
      + simpl in *. lia.
    - easy.
    - simpl. lia.
  Qed.
End LIFO_state_invariant.

CoreErlang Separate Extraction
  Counter.init Counter.handle_cast Counter.handle_call Counter.handle_info
  LIFO.init LIFO.handle_cast LIFO.handle_call LIFO.handle_info.
