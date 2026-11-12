-module(mpatho_suite).
-include_lib("eunit/include/eunit.hrl").

%% Map stress tests: the oracles are kernel-computed (vm_compute)
%% through the Rocq list-based Storage implementation, so each test
%% checks the native maps lowering against the reference semantics.
%% All folds are order-independent (maps:fold order is arbitrary).

mput3_test() ->
    ?assertEqual(mpatho:mput3_result(), mpatho:mput3(7)).

mremove_test() ->
    ?assertEqual(mpatho:mremove_result(), mpatho:mremove(1)),
    ?assertEqual(mpatho:mremove_absent_result(), mpatho:mremove(9)).

%% maps of maps
mnest_test() ->
    ?assertEqual(mpatho:mnest_22_result(), mpatho:mnest(22)),
    ?assertEqual(mpatho:mnest_33_result(), mpatho:mnest(33)).

%% 50-entry map built by recursion
mbig_test() ->
    ?assertEqual(mpatho:mbig_result(), mpatho:mbig()).

%% fold building a new map, then folded again
mchain_test() ->
    M = maps:from_list([{1, 10}, {2, 20}]),
    ?assertEqual(mpatho:mchain_result(), mpatho:mchain(M)).

%% find over mixed present/absent keys with branching
msumkeys_test() ->
    M = maps:from_list([{1, 10}, {3, 30}, {5, 50}]),
    ?assertEqual(mpatho:msumkeys_result(),
                 mpatho:msumkeys([1, 2, 3, 4, 5], M)).

%% one map value shared by two folds
mshare_test() ->
    ?assertEqual(mpatho:mshare_result(), mpatho:mshare()).

%% partially applied put at various positions
mpartial_test() ->
    M = mpatho:mpartial(mpatho:mbuild(5)),
    Sum = maps:fold(fun(_K, V, A) -> V + A end, 0, M),
    ?assertEqual(mpatho:mpartial_result(), Sum).
