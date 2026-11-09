-module(curry_tests).
-include_lib("eunit/include/eunit.hrl").

nat(0) -> 'rocq_O';
nat(N) -> {'rocq_S', nat(N - 1)}.

lst([]) -> 'rocq_nil';
lst([H | T]) -> {'rocq_cons', H, lst(T)}.

add12_test() ->
    ?assertEqual(nat(6), rocq_curry:rocq_add12(nat(3))).

succ_then_add2_test() ->
    ?assertEqual(nat(5), rocq_curry:rocq_succ_then_add2(nat(2))).

map_add2_test() ->
    ?assertEqual(lst([nat(2), nat(3), nat(4)]),
                 rocq_curry:rocq_map_add2(lst([nat(0), nat(1), nat(2)]))).

twice_curried_test() ->
    ?assertEqual(nat(3), rocq_curry:rocq_twice_curried(nat(1))).
