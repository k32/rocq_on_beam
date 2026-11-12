-module(poly_suite).
-include_lib("eunit/include/eunit.hrl").

%% nat lowers to native integers now
nat(N) -> N.

%% type parameters are erased: the extracted functions are untyped,
%% so they work on arbitrary 2-tuples {X, Y} (prod lowers to a
%% native, untagged tuple).

myfst_test() ->
    ?assertEqual(nat(1), poly:myfst({nat(1), 'x'})).

mysnd_test() ->
    ?assertEqual('x', poly:mysnd({nat(1), 'x'})).

swap_test() ->
    ?assertEqual({nil, nat(1)}, poly:swap({nat(1), nil})).

%% even a type parameter instantiated with a function disappears:
%% the function argument is applied dynamically
succ(X) -> X + 1.

mapfst_test() ->
    ?assertEqual({nat(1), 'tag'},
                 poly:mapfst(fun succ/1, {nat(0), 'tag'})).

%% the same untyped myfst works on non-nat components
untyped_test() ->
    ?assertEqual(apple, poly:myfst({apple, banana})).
