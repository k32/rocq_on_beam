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

%% the real thing: the extracted Counter behind the OTP gen_server,
%% via the coq_gen_server shim
lifo_test() ->
    {ok, Pid} = coq_gen_server:start_link('gen_server.LIFO', [1, 2]),
    try
        ?assertEqual(2, gen_server:call(Pid, get_len)),
        Pid ! {flush, false},
        timer:sleep(100),
        ?assertEqual(0, gen_server:call(Pid, get_len)),
        ?assertEqual('None', gen_server:call(Pid, pop)),
        _ = gen_server:call(Pid, {push, 1}),
        _ = gen_server:cast(Pid, {apush, 2}),
        timer:sleep(100),
        ?assertEqual(2, gen_server:call(Pid, get_len)),
        ?assertEqual({'Some', 2}, gen_server:call(Pid, pop)),
        ?assertEqual({'Some', 1}, gen_server:call(Pid, pop)),
        ?assertEqual('None', gen_server:call(Pid, pop)),
        _ = gen_server:call(Pid, {push, 1}),
        _ = gen_server:cast(Pid, adrop),
        timer:sleep(100),
        ?assertEqual(0, gen_server:call(Pid, get_len))
    after
        ok = gen_server:stop(Pid)
    end.
