%% ANVL project: the Rocq-to-Core-Erlang extraction test pipeline.
%%
%% dune remains the source of truth for the OCaml plugin and the Rocq
%% theories. Every test is a separate OTP application
%% (test/extracted/<base>): its extracted .core files (rocq writes to
%% its cwd) plus its eunit suite. Test execution is driven by the
%% anvl_test plugin (git dependency): the eunit suites of an
%% application are discovered automatically and each suite module runs
%% as a test case on its own node, with a code path built from the
%% application's closure. Since every CoreErlang Separate Extraction
%% command is its own project, two tests may emit different versions
%% of the same dependency module (e.g. 'Corelib.Init.Nat' with
%% different closures) -- being separate applications running on
%% separate nodes, those versions never meet.
-export([conf/0]).

-include("anvl.hrl").

conf() ->
  #{ plugins => [anvl_erlc, anvl_git, anvl_test]
   , conditions => [all]
   , [deps, git] =>
       [ #{ id => anvl_test
          , repo => "https://github.com/k32/anvl_test.git"
          , ref => {branch, "main"}
          }
       , #{ id => familiar
          , repo => "https://github.com/ieQu1/familiar.git"
          , ref => {tag, "0.1.6"}
          }
       ]
   , erlang =>
       #{ app_paths => ["runtime", "test/extracted/${app}"]
        , sources => ["${src_root}/src/*.erl", "${src_root}/src/*.core"]
        , includes => ["${src_root}/src", anvl_plugin:includes_dir()]
        }
   }.

%% Repo root: this is the root project
repo_root() -> anvl_project:root_dir().

theories_dir() ->
  filename:join(repo_root(), "_build/default/theories").

plugin_dir() ->
  filename:join([repo_root(), "test", "plugin"]).

test_dir() -> filename:join(repo_root(), "test").

suite_src_dir() -> filename:join(test_dir(), "src").

extracted_root() ->
  filename:join([repo_root(), "test", "extracted"]).

app_dir(Base) -> filename:join(extracted_root(), Base).

v_files() ->
  filelib:wildcard(filename:join(test_dir(), "*.v")).

bases() ->
  lists:sort(
    [filename:rootname(filename:basename(V)) || V <- v_files()]).

?MEMO(build_inputs,
      begin
        %% Plugin + theories: dune decides what to rebuild
        _ = anvl_lib:exec("dune", ["build"], [{cd, repo_root()}]),
        false
      end).

%% One test/<base>.v -> the OTP application test/extracted/<base>:
%% its .core files (written by rocq into the app's src dir), its
%% .app.src and its eunit suite.
?MEMO(app_ready, Base,
      begin
        _ = precondition(build_inputs()),
        VFile = filename:join(test_dir(), Base ++ ".v"),
        SrcDir = filename:join(app_dir(Base), "src"),
        Core = filename:join(SrcDir, Base ++ ".core"),
        %% the plugin artifact participates: a plugin rebuild must
        %% re-extract even when the .v file is unchanged
        Plugin = filename:join(plugin_dir(), "coreerlang_plugin.cmxs"),
        Reextract = anvl_lib:newer([VFile, Plugin], Core),
        Reextract
          andalso begin
            %% wipe the stale .core files: dropped dependencies of an
            %% older version of the test must not linger in the app
            _ = [file:delete(F)
                 || F <- filelib:wildcard(filename:join(SrcDir, "*.core"))],
            ok = filelib:ensure_path(SrcDir),
            _ = anvl_lib:exec(
                  "rocq",
                  ["c", "-q",
                   "-I", plugin_dir(),
                   "-Q", theories_dir(), "Erlang",
                   VFile],
                  [{cd, SrcDir}]),
            true
          end,
        %% application spec and suite are (re)written by content
        ok = filelib:ensure_path(SrcDir),
        AppSrc =
          io_lib:format(
            "~p.~n",
            [{application, list_to_atom(Base),
              [{description, "Extracted Core Erlang modules"},
               {vsn, "0.1.0"}, {registered, []},
               %% erlang_runtime: the Erlang-side support modules
               %% (e.g. the gen_server shim) reach the test nodes'
               %% code path via the application closure
               {applications, [kernel, stdlib, eunit, erlang_runtime]}]}]),
        changed_write(filename:join(SrcDir, Base ++ ".app.src"),
                      unicode:characters_to_binary(AppSrc)),
        %% the eunit suite of the test (named <base>_suite: the
        %% anvl_test eunit discovery skips modules called <base>_tests
        %% when <base> itself is in the same application -- and it
        %% always is here, as the extracted module of the test)
        {ok, SuiteSrc} =
          file:read_file(filename:join(suite_src_dir(), Base ++ "_suite.erl")),
        changed_write(filename:join(SrcDir, Base ++ "_suite.erl"), SuiteSrc),
        Reextract
      end).

%% write only when the content actually changed; true when written
changed_write(File, Content) ->
  case file:read_file(File) of
    {ok, Content} -> false;
    _ ->
      ok = file:write_file(File, Content),
      true
  end.

%% Run the eunit suites of every test application via anvl_test: each
%% suite module is a test case executing on its own node. Stale app
%% dirs of removed tests are cleaned up.
?MEMO(all,
      begin
        [_ | _] = Bases = bases(),
        _ = precondition([app_ready(B) || B <- Bases]),
        _ = [file:del_dir_r(app_dir(D))
             || D <- filelib:wildcard(filename:join(extracted_root(), "*")),
                filename:basename(D) =/= ".",
                not lists:member(filename:basename(D), Bases)],
        lists:foldl(
          fun(B, Acc) ->
              anvl_test:run(default, list_to_atom(B)) orelse Acc
          end, false, Bases)
      end).
