-module(z_tests).
-include_lib("eunit/include/eunit.hrl").

%% literals fold to integers
fourtytwo_test() ->
    ?assertEqual(42, z:fourtytwo()),
    ?assertEqual(-7, z:minus7()),
    ?assertEqual(123456789012345678901234567890, z:big()).

%% library ops map to Erlang BIFs (arbitrary precision integers)
ops_test() ->
    ?assertEqual(30, z:sum(10, 20)),
    ?assertEqual(-30, z:sum(10, -40)),
    ?assertEqual(123456789012345678901234567890000000,
                 z:prod(z:big(), 1000000)),
    ?assertEqual(20, z:mx(20, 10)).

absdiff_test() ->
    ?assertEqual(15, z:absdiff(10, 25)),
    ?assertEqual(15, z:absdiff(25, 10)).

%% boolean results are the native atoms
eq_test() ->
    ?assertEqual('true', z:eq(3, 3)),
    ?assertEqual('false', z:eq(3, 4)).

%% partially applied library function (eta-expanded at use site)
half_away_test() ->
    ?assertEqual(9, z:half_away(10)).

%% conversions between nat and Z are the identity (both are integers)
conv_test() ->
    ?assertEqual(5, z:ofn(5)),
    ?assertEqual(7, z:ton(7)).

%% user matches on the Z structure rewrite to guarded clauses
sgn_test() ->
    ?assertEqual(0, z:sgn(0)),
    ?assertEqual(1, z:sgn(99)),
    ?assertEqual(-1, z:sgn(-99)).
