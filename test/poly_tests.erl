-module(poly_tests).
-include_lib("eunit/include/eunit.hrl").

nat(0) -> 'rocq_O';
nat(N) -> {'rocq_S', nat(N - 1)}.

%% type parameters are erased: the extracted functions are untyped,
%% so they work on arbitrary 2-tuples {'rocq_pair', X, Y}.

myfst_test() ->
    ?assertEqual(nat(1), rocq_poly:rocq_myfst({'rocq_pair', nat(1), 'x'})).

mysnd_test() ->
    ?assertEqual('x', rocq_poly:rocq_mysnd({'rocq_pair', nat(1), 'x'})).

swap_test() ->
    ?assertEqual({'rocq_pair', nil, nat(1)},
                 rocq_poly:rocq_swap({'rocq_pair', nat(1), nil})).

%% even a type parameter instantiated with a function disappears:
%% the function argument is applied dynamically
succ(X) -> {'rocq_S', X}.

mapfst_test() ->
    ?assertEqual({'rocq_pair', nat(1), 'tag'},
                 rocq_poly:rocq_mapfst(fun succ/1, {'rocq_pair', nat(0), 'tag'})).

%% the same untyped myfst works on non-nat components
untyped_test() ->
    ?assertEqual(apple, rocq_poly:rocq_myfst({'rocq_pair', apple, banana})).
