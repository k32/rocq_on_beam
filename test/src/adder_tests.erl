-module(adder_tests).
-include_lib("eunit/include/eunit.hrl").

%% nat lowers to native integers
nat(N) -> N.

add_test() ->
    ?assertEqual(nat(2), adder:add(nat(2), nat(0))),
    ?assertEqual(nat(5), adder:add(nat(2), nat(3))),
    ?assertEqual(nat(3), adder:add(nat(0), nat(3))).

add_zero_left_test() ->
    ?assertEqual(nat(7), adder:add(nat(0), nat(7))).
