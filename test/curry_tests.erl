-module(curry_tests).
-include_lib("eunit/include/eunit.hrl").

nat(0) -> 'O';
nat(N) -> {'S', nat(N - 1)}.

lst([]) -> 'nil';
lst([H | T]) -> {'cons', H, lst(T)}.

add12_test() ->
    ?assertEqual(nat(6), curry:add12(nat(3))).

succ_then_add2_test() ->
    ?assertEqual(nat(5), curry:succ_then_add2(nat(2))).

map_add2_test() ->
    ?assertEqual(lst([nat(2), nat(3), nat(4)]),
                 curry:map_add2(lst([nat(0), nat(1), nat(2)]))).

twice_curried_test() ->
    ?assertEqual(nat(3), curry:twice_curried(nat(1))).
