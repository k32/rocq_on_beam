Class FromT {from_t : Set} :=
  {}.

Class GenServerT :=
  { init_arg_t : Set;
    state_t : Set;
    exit_reason_t : Set;

    call_t : Set;
    cast_t : Set;
    info_t : Set;

    reply_t : call_t -> Set;
  }.

Inductive init_res `{GenServerT} :=
| i_ok : state_t -> init_res
| i_stop : exit_reason_t -> init_res
| i_ignore.

Inductive handle_cast_res `{GenServerT} :=
| cst_noreply : state_t -> handle_cast_res
| cst_stop : exit_reason_t -> state_t -> handle_cast_res.

Inductive call_res `{GenServerT} (call : call_t) :=
| c_reply : reply_t call -> state_t -> call_res call
| c_stop_reply : exit_reason_t -> reply_t call -> state_t -> call_res call
| c_stop : exit_reason_t -> state_t -> call_res call.

Inductive info_res `{GenServerT} :=
| inf_noreply : state_t -> info_res
| inf_stop : exit_reason_t -> state_t -> info_res.

Module Type gen_server.
  Parameter t : GenServerT.

  Parameter init : @init_arg_t t -> @init_res t.

  Parameter handle_cast : @cast_t t -> @state_t t -> @handle_cast_res t.

  Parameter handle_call : forall `{FromT} (call : @call_t t), from_t -> @state_t t -> @call_res t call.

  Parameter handle_info : @info_t t -> @state_t t -> @info_res t.
End gen_server.
