-module(gen_server_suite).
-include_lib("eunit/include/eunit.hrl").

%% two gen_servers, each its own dotted Erlang module

counter_init_test() ->
    ?assertEqual({i_ok, 5}, 'gen_server.Counter':init(5)).

counter_cast_test() ->
    ?assertEqual({cst_noreply, 6},
                 'gen_server.Counter':handle_cast(inc, 5)),
    ?assertEqual({'cst_noreply', 4},
                 'gen_server.Counter':handle_cast(dec, 5)).

counter_call_test() ->
    ?assertEqual({c_reply, 5, 5},
                 'gen_server.Counter':handle_call(get, from, 5)),
    ?assertEqual({c_reply, 2, 7},
                 'gen_server.Counter':handle_call({add, 2}, from, 5)).

%% cinfo is a single-constructor inductive: it extracts to the bare
%% payload (record optimization), so Info values are plain integers
counter_info_test() ->
    ?assertEqual({inf_noreply, 3},
                 'gen_server.Counter':handle_info(3, 9)).

lifo_test() ->
    {ok, Pid} = rocq_gen_server:start_link('gen_server.LIFO', [1, 2]),
    try
        ?assertEqual(2, 'gen_server.LIFO':get_len(Pid)),
        Pid ! {flush, false},
        timer:sleep(100),
        ?assertEqual(0, 'gen_server.LIFO':get_len(Pid)),
        ?assertEqual('None', 'gen_server.LIFO':pop(Pid)),
        _ = 'gen_server.LIFO':push(Pid, 1),
        _ = 'gen_server.LIFO':apush(Pid, 2),
        timer:sleep(100),
        ?assertEqual(2, 'gen_server.LIFO':get_len(Pid)),
        ?assertEqual({'Some', 2}, 'gen_server.LIFO':pop(Pid)),
        ?assertEqual({'Some', 1}, 'gen_server.LIFO':pop(Pid)),
        ?assertEqual('None', 'gen_server.LIFO':pop(Pid)),
        _ = 'gen_server.LIFO':push(Pid, 1),
        _ = 'gen_server.LIFO':adrop(Pid),
        timer:sleep(100),
        ?assertEqual(0, 'gen_server.LIFO':get_len(Pid))
    after
        ok = gen_server:stop(Pid)
    end.
