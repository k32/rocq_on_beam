-module(nat_tests).
-include_lib("eunit/include/eunit.hrl").

%% nat lowers to native integers (S-chains fold into one literal)
big_test() ->
    ?assertEqual(512, nat:big()).

three_test() ->
    ?assertEqual(3, nat:three()).

%% matches on O/S become <0> / guarded clauses with a predecessor let
predn_test() ->
    ?assertEqual(0, nat:predn(0)),
    ?assertEqual(4, nat:predn(5)).

%% direct interop with Erlang arithmetic on both sides
interop_test() ->
    ?assertEqual(7, nat:predn(2 + 6)).
