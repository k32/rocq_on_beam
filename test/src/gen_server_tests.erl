-module(gen_server_tests).
-include_lib("eunit/include/eunit.hrl").

%% two gen_servers, each its own dotted Erlang module

counter_init_test() ->
    ?assertEqual({'i_ok', 5}, 'gen_server.Counter':init(5)).

counter_cast_test() ->
    ?assertEqual({'cst_noreply', 6},
                 'gen_server.Counter':handle_cast('inc', 5)),
    ?assertEqual({'cst_noreply', 4},
                 'gen_server.Counter':handle_cast('dec', 5)).

toggle_test() ->
    ?assertEqual({'i_ok', 'true'}, 'gen_server.Toggle':init('true')),
    ?assertEqual({'cst_noreply', 'false'},
                 'gen_server.Toggle':handle_cast('flip', 'true')),
    ?assertEqual({'cst_noreply', 'true'},
                 'gen_server.Toggle':handle_cast('noop', 'true')).
