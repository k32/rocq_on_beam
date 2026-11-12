Inductive init_res {State ExitReason} :=
| i_ok : State -> init_res
| i_stop : ExitReason -> init_res
| i_ignore.

Inductive handle_cast_res {State ExitReason} :=
| cst_noreply : State -> handle_cast_res
| cst_stop : ExitReason -> State -> handle_cast_res.

Module Type gen_server.
  Parameter InitArg : Set.

  Parameter ExitReason : Set.

  Parameter State : Set.

  Parameter Cast : Set.

  Parameter init : InitArg -> @init_res State ExitReason.

  Parameter handle_cast : Cast -> State -> @handle_cast_res State ExitReason.
End gen_server.
