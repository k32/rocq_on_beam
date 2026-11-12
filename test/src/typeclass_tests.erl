-module(typeclass_tests).
-include_lib("eunit/include/eunit.hrl").

%% instances are nullary constants of record type; the dictionary
%% argument is resolved at elaboration time, so no special lowering
methods_test() ->
    ?assertEqual(0, typeclass:describe(-42)),
    ?assertEqual(42 + 7, typeclass:both(-42, 7)).

%% superclass dictionaries are nested record fields
superclass_test() ->
    ?assertEqual(2, typeclass:describe2(-42)).

%% a generic class function takes the dictionary as an explicit
%% argument (the extracted 'show_Z' record)
generic_test() ->
    Dict = {'Build_Show', fun(_) -> 9 end, fun(N) -> N + 1 end},
    ?assertEqual(18, typeclass:showTwice(Dict, 5)).
