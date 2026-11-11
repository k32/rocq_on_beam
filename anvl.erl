%% ANVL project: extract the test definitions from Rocq to Core Erlang,
%% compile everything (the extracted .core modules with from_core, the
%% .erl suites via anvl_erlc) and run eunit.
%%
%% dune remains the source of truth for the OCaml plugin and the Rocq
%% theories: the [build_inputs] condition runs `dune build` (a fast
%% no-op when fresh). The plugin is staged for findlib under
%% test/plugin/. Every test/<base>.v produces
%% test/extracted/src/<base>.core, compiled to
%% test/extracted/ebin/<base>.beam with compile:file/2 + from_core.

-export([conf/0]).

-include("anvl.hrl").

conf() ->
  #{plugins => [anvl_erlc],
    conditions => [all],
    deps =>
      #{local =>
          [#{kind => otp_application, dir => "test"}]}}.

test_dir() -> filename:join(anvl_project:root_dir(), "test").

theories_dir() ->
  filename:join(anvl_project:root_dir(), "_build/default/theories").

plugin_dir() -> filename:join(test_dir(), "plugin").

bases() ->
  filelib:wildcard(filename:join(test_dir(), "*.v")),
  lists:map(fun(V) -> filename:rootname(filename:basename(V)) end,
            filelib:wildcard(filename:join(test_dir(), "*.v"))).

v_file(Base) -> filename:join(test_dir(), Base ++ ".v").

core_file(Base) ->
  filename:join([test_dir(), "extracted", "src", Base ++ ".core"]).

beam_file(Base) ->
  filename:join([test_dir(), "extracted", "ebin", Base ++ ".beam"]).

?MEMO(build_inputs,
      begin
        %% Plugin + theories: dune decides what to rebuild
        _ = anvl_lib:exec("dune", ["build"],
                          [{cd, anvl_project:root_dir()}]),
        false
      end).

?MEMO(extracted, Base,
      begin
        _ = precondition(build_inputs()),
        ok = filelib:ensure_path(filename:dirname(core_file(Base))),
        case anvl_lib:newer([v_file(Base)], core_file(Base)) of
          true ->
            %% rocq resolves the CoreErlang Extraction "base" argument
            %% relative to its cwd, so run it inside extracted/src
            _ = anvl_lib:exec(
                  "rocq",
                  ["c", "-q",
                   "-I", plugin_dir(),
                   "-Q", theories_dir(), "Erlang",
                   v_file(Base)],
                  [{cd, filename:dirname(core_file(Base))}]),
            true;
          false ->
            false
        end
      end).

%% Compile one extracted Core Erlang module (anvl_erlc's dependency
%% scanner assumes Erlang sources, so .core files cannot go through
%% it)
?MEMO(core_compiled, Base,
      begin
        _ = precondition(extracted(Base)),
        ok = filelib:ensure_path(filename:dirname(beam_file(Base))),
        case anvl_lib:newer([core_file(Base)], beam_file(Base)) of
          true ->
            case compile:file(core_file(Base),
                              [from_core, debug_info,
                               {outdir, filename:dirname(beam_file(Base))},
                               return]) of
              {ok, _, []} ->
                  true;
              {ok, _, Ws0} ->
                  %% sys_core_fold emits benign {ignored, _} notices
                  %% on extracted code
                  IsBenign = fun
                      ({_, [{_, sys_core_fold, {ignored, _}}]}) -> true;
                      (_) -> false
                  end,
                  Ws = lists:filter(fun(W) -> not IsBenign(W) end, Ws0),
                  case Ws of
                      [] -> true;
                      _ -> exit(unsat)
                  end;
              {error, _, _} ->
                  exit(unsat)
            end;
          false ->
            false
        end
      end).

%% Compile the eunit suites
?MEMO(compile,
      precondition(
        [anvl_erlc:app_compiled(default, rocq_core_erlang_test) |
         [core_compiled(Base) || Base <- bases()]])).

%% Run the aggregator suite. The flag of [compile] is deliberately
%% ignored: tests run on every invocation. anvl_erlc may have loaded
%% older module versions into this node while analyzing them, so the
%% code server cache is purged and reloaded from the beam files first.
?MEMO(all,
      begin
        _ = precondition(compile()),
        CoreEbin = filename:dirname(beam_file("x")),
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
          end, [SuiteEbin, CoreEbin]),
        case eunit:test(rocq_core_erlang_test, [verbose]) of
          ok -> true;
          error -> exit(unsat)
        end
      end).
