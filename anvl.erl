%% ANVL project: the Rocq-to-Core-Erlang extraction test pipeline.
%%
%% dune remains the source of truth for the OCaml plugin and the Rocq
%% theories. The extracted .core modules are built by the child
%% project in test/extracted (pre-compile hook runs rocq, anvl_erlc
%% compiles with from_core); the .erl suites live in the test app.
%% The `all` condition runs the aggregator eunit suite.
-export([conf/0]).

-include("anvl.hrl").

conf() ->
  #{plugins => [anvl_erlc],
    conditions => [all],
    deps =>
      #{local =>
          [#{kind => otp_application, dir => "test/extracted"},
           #{kind => otp_application, dir => "test"}]}}.

test_dir() -> filename:join(anvl_project:root_dir(), "test").

%% Compile the extracted modules and the eunit suites
?MEMO(compile,
      precondition(
        [ anvl_erlc:app_compiled(default, extracted),
          anvl_erlc:app_compiled(default, rocq_core_erlang_test)
        ])).

%% Run the aggregator suite. The flag of [compile] is deliberately
%% ignored: tests run on every invocation. anvl_erlc may have loaded
%% older module versions into this node while analyzing them, so the
%% code server cache is purged and reloaded from the beam files first.
?MEMO(all,
      begin
        _ = precondition(compile()),
        CoreEbin = anvl_erlc:app_path(default, extracted),
        SuiteEbin = anvl_erlc:app_path(default, rocq_core_erlang_test),
        lists:foreach(
          fun(Ebin) ->
              true = code:add_patha(Ebin),
              lists:foreach(
                fun(Beam) ->
                    M = list_to_atom(filename:rootname(filename:basename(Beam))),
                    _ = code:purge(M),
                    _ = code:delete(M),
                    {module, M} = code:ensure_loaded(M)
                end, filelib:wildcard(filename:join(Ebin, "*.beam")))
          end, [CoreEbin, SuiteEbin]),
        case eunit:test(rocq_core_erlang_test, [verbose]) of
          ok -> true;
          error -> exit(unsat)
        end
      end).
