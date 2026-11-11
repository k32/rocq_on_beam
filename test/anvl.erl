%% ANVL project: extract the test definitions from Rocq to Core Erlang,
%% compile everything (the extracted .core modules with from_core, the
%% .erl suites via anvl_erlc) and run eunit.
%%
%% dune remains the source of truth for the OCaml plugin and the Rocq
%% theories: the [build_inputs] condition runs `dune build` (a fast
%% no-op when fresh). The plugin is staged for findlib under plugin/.
%% Every test/<base>.v produces extracted/src/<base>.core, compiled to
%% extracted/ebin/<base>.beam with compile:file/2 + from_core.
-module(anvl).

-export([conf/0]).

-include("anvl.hrl").

conf() ->
  #{plugins => [anvl_erlc],
    conditions => [all]}.

%% Root = the directory containing this file (= test/); the repository
%% root is one level up.
repo_root() -> filename:dirname(anvl_project:root_dir()).

theories_dir() -> filename:join(repo_root(), "_build/default/theories").

plugin_dir() -> filename:join(anvl_project:root_dir(), "plugin").

v_files() ->
  filelib:wildcard(filename:join(anvl_project:root_dir(), "*.v")).

core_file(Base) ->
  filename:join([anvl_project:root_dir(), "extracted", "src", Base ++ ".core"]).

beam_file(Base) ->
  filename:join([anvl_project:root_dir(), "extracted", "ebin", Base ++ ".beam"]).

?MEMO(build_inputs,
      begin
        %% Plugin + theories: dune decides what to rebuild
        _ = anvl_lib:exec("dune", ["build"], [{cd, repo_root()}]),
        _ = anvl_lib:exec("mkdir",
                          ["-p", filename:dirname(core_file("x")),
                           filename:dirname(beam_file("x"))],
                          [{cd, anvl_project:root_dir()}]),
        false
      end).

?MEMO(extraction,
      begin
        _ = precondition(build_inputs()),
        lists:foreach(
          fun(VFile) ->
              Base = filename:rootname(filename:basename(VFile)),
              Core = core_file(Base),
              case anvl_lib:newer([VFile], Core) of
                true ->
                  %% rocq resolves the CoreErlang Extraction "base"
                  %% argument relative to its cwd, so run it inside
                  %% extracted/src
                  _ = anvl_lib:exec("rocq",
                                    ["c", "-q",
                                     "-I", plugin_dir(),
                                     "-Q", theories_dir(), "Erlang",
                                     VFile],
                                    [{cd, filename:dirname(Core)}]);
                false ->
                  ok
              end
          end, v_files()),
        true
      end).

%% Compile the extracted Core Erlang modules (anvl_erlc's dependency
%% scanner assumes Erlang sources, so .core files are compiled here
%% directly)
?MEMO(cores_compiled,
      begin
        _ = precondition(extraction()),
        lists:foreach(
          fun(Core) ->
              Beam = filename:join(
                       filename:dirname(beam_file("x")),
                       filename:basename(Core, ".core") ++ ".beam"),
              case anvl_lib:newer([Core], Beam) of
                true ->
                  case compile:file(Core, [from_core, debug_info,
                                           {outdir, filename:dirname(Beam)},
                                           return]) of
                    {ok, _, []} ->
                      ok;
                    {ok, _, Ws0} ->
                      %% sys_core_fold emits benign {ignored, _}
                      %% notices on extracted code
                      IsBenign = fun
                          ({_, [{_, sys_core_fold, {ignored, _}}]}) -> true;
                          (_) -> false
                      end,
                      Ws = lists:filter(fun(W) -> not IsBenign(W) end, Ws0),
                      case Ws of
                        [] -> ok;
                        _ -> ?UNSAT("~p: ~p", [Core, Ws])
                      end;
                    {error, Es, _} ->
                      ?UNSAT("~p: ~p", [Core, Es])
                  end;
                false ->
                  ok
              end
          end, filelib:wildcard(core_file("*"))),
        true
      end).

%% Compile the eunit suites
?MEMO(compile,
      precondition(
        [ cores_compiled(),
          anvl_erlc:app_compiled(default, rocq_core_erlang_test)
        ])).

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
          end, [CoreEbin, SuiteEbin]),
        case eunit:test(rocq_core_erlang_test, [verbose]) of
          ok -> true;
          error -> ?UNSAT("eunit failed", [])
        end
      end).
