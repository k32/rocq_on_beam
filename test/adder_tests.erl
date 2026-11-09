-module(adder_tests).
-include_lib("eunit/include/eunit.hrl").

%% Peano naturals as extracted from Rocq: 'rocq_O' | {'rocq_S', N}
nat(0) -> 'rocq_O';
nat(N) -> {'rocq_S', nat(N - 1)}.

add_test() ->
    ?assertEqual(nat(2), rocq_adder:rocq_add(nat(2), nat(0))),
    ?assertEqual(nat(5), rocq_adder:rocq_add(nat(2), nat(3))),
    ?assertEqual(nat(3), rocq_adder:rocq_add(nat(0), nat(3))).

add_zero_left_test() ->
    ?assertEqual(nat(7), rocq_adder:rocq_add(nat(0), nat(7))).
