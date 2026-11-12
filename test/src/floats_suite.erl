-module(floats_suite).
-include_lib("eunit/include/eunit.hrl").

%% primitive floats lower to native Erlang float literals
literals_test() ->
    ?assertEqual(2.5, floats:e()),
    ?assertEqual(0.125, floats:small()),
    ?assertEqual(-1.5, floats:neg()).
