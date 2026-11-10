-module(mcurry_tests).
-include_lib("eunit/include/eunit.hrl").

%% curried HMap methods: fold/find/remove partially applied and passed
%% around as first-class functions; oracles kernel-computed

cfold_test() ->
    M = maps:from_list([{1, 10}, {2, 20}]),
    ?assertEqual(mcurry:cfold_result(), mcurry:cfold(M)).

cfind_test() ->
    %% note: the maps extracted from the .v are built by maps:put
    L = [mcurry:m1(), #{}, mcurry:m2()],
    ?assertEqual(mcurry:cfind_result(), mcurry:cfind(1, L)).

cremove_test() ->
    %% cremove_result maps (HMap.find 1) over the removed maps; both
    %% should miss afterwards
    ?assertEqual(mcurry:cremove_result(),
                 [maps:find(1, mcurry:cremove(mcurry:m1())),
                  maps:find(1, mcurry:cremove(mcurry:m2()))]).
