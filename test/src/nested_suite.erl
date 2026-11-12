-module(nested_suite).
-include_lib("eunit/include/eunit.hrl").

%% nat lowers to native integers now
nat(N) -> N.

%% let-bound lambdas nested inside each other
nested_test() ->
    %% f x = x+1; g x = f (f x); nested n = h (f n) = g (n+1) = n+3
    ?assertEqual(nat(7), nested:nested(nat(4))),
    ?assertEqual(nat(3), nested:nested(nat(0))).

%% a lambda immediately applied to several arguments
applied_test() ->
    %% 10*n + 3
    ?assertEqual(nat(23), nested:applied(nat(2))),
    ?assertEqual(nat(3), nested:applied(nat(0))).

%% a let-bound recursive function (fix inside a let)
deep_test() ->
    ?assertEqual(nat(1), nested:deep(nat(0))),
    ?assertEqual(nat(16), nested:deep(nat(4))).

%% inner binder shadowing the outer parameter
shadow_test() ->
    %% (n+1) + nested 0 = n + 1 + 3
    ?assertEqual(nat(7), nested:shadow(nat(3))).
