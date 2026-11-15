-module(native_call_suite).
-include_lib("eunit/include/eunit.hrl").

%% CoreErlang Native Call: a user definition mapped onto an Erlang
%% function renders every application as a call to that function
mul_test() ->
    ?assertEqual(12, native_call:twelve()),
    ?assertEqual(42, native_call:m(6, 7)).

%% the leading Rocq argument is dropped untranslated; the remaining
%% ones go to the Erlang function (here unary erlang:abs), so the
%% extracted wrapper is itself unary
drop_test() ->
    ?assertEqual(5, native_call:five()),
    ?assertEqual(7, native_call:d(7)).
