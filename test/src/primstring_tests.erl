-module(primstring_tests).
-include_lib("eunit/include/eunit.hrl").

%% primitive strings lower to native Erlang binaries
greeting_test() ->
    ?assertEqual(<<"hi">>, primstring:greeting()).
