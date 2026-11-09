-module(poly_tests).
-include_lib("eunit/include/eunit.hrl").

nat(0) -> 'O';
nat(N) -> {'S', nat(N - 1)}.

%% type parameters are erased: the extracted functions are untyped,
%% so they work on arbitrary 2-tuples {'pair', X, Y}.

myfst_test() ->
    ?assertEqual(nat(1), poly:myfst({'pair', nat(1), 'x'})).

mysnd_test() ->
    ?assertEqual('x', poly:mysnd({'pair', nat(1), 'x'})).

swap_test() ->
    ?assertEqual({'pair', nil, nat(1)},
                 poly:swap({'pair', nat(1), nil})).

%% even a type parameter instantiated with a function disappears:
%% the function argument is applied dynamically
succ(X) -> {'S', X}.

mapfst_test() ->
    ?assertEqual({'pair', nat(1), 'tag'},
                 poly:mapfst(fun succ/1, {'pair', nat(0), 'tag'})).

%% the same untyped myfst works on non-nat components
untyped_test() ->
    ?assertEqual(apple, poly:myfst({'pair', apple, banana})).
