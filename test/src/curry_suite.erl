-module(curry_suite).
-include_lib("eunit/include/eunit.hrl").

%% nat lowers to native integers now
nat(N) -> N.

add12_test() ->
    ?assertEqual(nat(6), curry:add12(nat(3))).

succ_then_add2_test() ->
    ?assertEqual(nat(5), curry:succ_then_add2(nat(2))).

map_add2_test() ->
    ?assertEqual([nat(2), nat(3), nat(4)],
                 curry:map_add2([nat(0), nat(1), nat(2)])).

twice_curried_test() ->
    %% apply_both double succ 1 = double(1+1) = 4
    ?assertEqual(nat(4), curry:twice_curried(nat(1))).
