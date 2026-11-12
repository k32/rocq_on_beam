%% Erlang-side shim between extracted gen_servers (Rocq modules
%% ascribing to the Erlang.GenServer.gen_server module type) and the
%% OTP gen_server behaviour. The extracted callbacks return tagged
%% tuples/atoms named after the operation's result constructors
%% ('i_ok', 'c_reply', 'cst_noreply', ...); this module translates
%% them into the tuples OTP expects, and carries the callback module
%% alongside the user state.
-module(coq_gen_server).

-export([start/2, start_link/2, call/2, call/3, cast/2, stop/1,
         %% OTP gen_server callbacks (external calls need exports)
         init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Start an extracted gen_server callback module [Mod] with [Args].
start(Mod, Args) ->
    gen_server:start(?MODULE, {Mod, Args}, []).

start_link(Mod, Args) ->
    gen_server:start_link(?MODULE, {Mod, Args}, []).

call(Server, Req) ->
    gen_server:call(Server, Req).

call(Server, Req, Timeout) ->
    gen_server:call(Server, Req, Timeout).

cast(Server, Msg) ->
    gen_server:cast(Server, Msg).

stop(Server) ->
    gen_server:stop(Server).

%% --- OTP callbacks ----------------------------------------------------

init({Mod, Args}) ->
    case Mod:init(Args) of
        {'i_ok', State} -> {ok, {Mod, State}};
        {'i_stop', Reason} -> {stop, Reason};
        'i_ignore' -> ignore
    end.

handle_cast(Req, {Mod, State}) ->
    case Mod:handle_cast(Req, State) of
        {'cst_noreply', NewState} -> {noreply, {Mod, NewState}};
        {'cst_stop', Reason, NewState} -> {stop, Reason, {Mod, NewState}}
    end.

handle_call(Req, From, {Mod, State}) ->
    case Mod:handle_call(Req, From, State) of
        {'c_reply', Reply, NewState} -> {reply, Reply, {Mod, NewState}};
        {'c_noreply', NewState} -> {noreply, {Mod, NewState}};
        {'c_stop_reply', Reason, Reply, NewState} ->
            {stop, Reason, Reply, {Mod, NewState}};
        {'c_stop', Reason, NewState} -> {stop, Reason, {Mod, NewState}}
    end.

handle_info(Info, {Mod, State}) ->
    case Mod:handle_info(Info, State) of
        {'inf_noreply', NewState} -> {noreply, {Mod, NewState}};
        {'inf_stop', Reason, NewState} -> {stop, Reason, {Mod, NewState}}
    end.

terminate(_Reason, _State) ->
    ok.
