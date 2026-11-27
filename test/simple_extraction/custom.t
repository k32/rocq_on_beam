Extract:

  $ rocq compile -I ../../../install/default/lib/ custom.v > custom.erl

  $ cat custom.erl
  -module('custom').
  -export(['t_call_custom'/1]).
  
  't_call_custom'(_a@0) ->
    'erlang':'+'(_a@0, _a@0).
  
  


Compile and run:

  $ erlc custom.erl

  $ erl -pa . -noshell -eval 'io:format("~p~n", [custom:t_call_custom(1)]), halt().'
  2
