-module(gen_server_suite).
-include_lib("eunit/include/eunit.hrl").

%% two gen_servers, each its own dotted Erlang module

counter_init_test() ->
    ?assertEqual({'i_ok', 5}, 'gen_server.Counter':init(5)).

counter_cast_test() ->
    ?assertEqual({'cst_noreply', 6},
                 'gen_server.Counter':handle_cast('inc', 5)),
    ?assertEqual({'cst_noreply', 4},
                 'gen_server.Counter':handle_cast('dec', 5)).

counter_call_test() ->
    ?assertEqual({'c_reply', 5, 5},
                 'gen_server.Counter':handle_call('cget', from, 5)),
    ?assertEqual({'c_reply', 2, 7},
                 'gen_server.Counter':handle_call({'cadd', 2}, from, 5)).

%% cinfo is a single-constructor inductive: it extracts to the bare
%% payload (record optimization), so Info values are plain integers
counter_info_test() ->
    ?assertEqual({'inf_noreply', 3},
                 'gen_server.Counter':handle_info(3, 9)).

toggle_test() ->
    ?assertEqual({'i_ok', 'true'}, 'gen_server.Toggle':init('true')),
    ?assertEqual({'cst_noreply', 'false'},
                 'gen_server.Toggle':handle_cast('flip', 'true')),
    ?assertEqual({'cst_noreply', 'true'},
                 'gen_server.Toggle':handle_cast('noop', 'true')).

%% the real thing: the extracted Counter behind the OTP gen_server,
%% via the coq_gen_server shim
live_test() ->
    {ok, Pid} = coq_gen_server:start('gen_server.Counter', 5),
    try
        ?assertEqual(5, coq_gen_server:call(Pid, 'cget')),
        ?assertEqual(2, coq_gen_server:call(Pid, {'cadd', 2})),
        ?assertEqual(7, coq_gen_server:call(Pid, 'cget')),
        ok = coq_gen_server:cast(Pid, 'inc'),
        ok = coq_gen_server:cast(Pid, 'inc'),
        ?assertEqual(9, coq_gen_server:call(Pid, 'cget')),
        %% handle_info: plain messages reach the extracted callback
        %% (Info values are bare integers, see counter_info_test)
        Pid ! 42,
        timer:sleep(100),
        ?assertEqual(42, coq_gen_server:call(Pid, 'cget'))
    after
        ok = coq_gen_server:stop(Pid)
    end.
