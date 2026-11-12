-module(hmap_suite).
-include_lib("eunit/include/eunit.hrl").

%% the runtime representation is a native map; methods are isomorphic
%% maps module calls (find returns the native {ok, V} | error shape)
m1_test() ->
    ?assertEqual(#{1 => 10, 2 => 20}, hmap:m1()).

lookup_test() ->
    ?assertEqual({ok, 10}, hmap:lookup(1)),
    ?assertEqual(error, hmap:lookup(3)).

%% matching on MapFindResult from Rocq code lowers to the same shapes
get_or_zero_test() ->
    ?assertEqual(10, hmap:get_or_zero(1)),
    ?assertEqual(0, hmap:get_or_zero(3)).

add_test() ->
    ?assertEqual(#{1 => 10, 2 => 20, 3 => 30},
                 hmap:add(3, 30, hmap:m1())).

sum_test() ->
    ?assertEqual(50, hmap:sum()).
