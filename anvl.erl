%% ANVL project: the Rocq-to-Core-Erlang extraction test pipeline.
%%
%% dune remains the source of truth for the OCaml plugin and the Rocq
%% theories. Test execution is driven by the anvl_test plugin (git
%% dependency): every OTP application's eunit suites are discovered
%% and each eunit module runs as a test case on its own node, with a
%% code path built from the application's closure.
%%
%% Every CoreErlang Separate Extraction command is its own project, so
%% two tests may emit different versions of the same dependency module
%% (e.g. 'Corelib.Init.Nat' with different closures). Tests are grouped
%% by greedy coloring over shared module names: tests sharing a module
%% land in different generated apps (test/extracted/g<N>, holding the
%% extracted .core files and the eunit suites of its tests), so the
%% conflicting versions never meet on a test node.
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
       #{ app_paths => ["test/extracted/${app}"]
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

%% Per-test staging: rocq writes the .core files of one test into its
%% own directory, so parallel extractions cannot race over shared
%% dependency modules.
stage_root() -> filename:join(extracted_root(), "stage").

stage_dir(Base) -> filename:join(stage_root(), Base).

group_dir(G) -> filename:join(extracted_root(), G).

v_files() ->
  filelib:wildcard(filename:join(test_dir(), "*.v")).

bases() ->
  lists:sort(
    [filename:rootname(filename:basename(V)) || V <- v_files()]).

%% module names produced by one test's extraction
stage_modules(Base) ->
  [ filename:rootname(filename:basename(F))
    || F <- filelib:wildcard(filename:join(stage_dir(Base), "*.core")) ].

%% Assign each test a group ("g1", "g2", ...): two tests sharing any
%% module name get different groups (greedy coloring over the sharing
%% graph; tests are visited in sorted order, so the result is
%% deterministic). Returns #{base => Group} and the group list.
group_map() ->
  Bases = bases(),
  Ms = maps:from_list([{B, stage_modules(B)} || B <- Bases]),
  Owners =
    lists:foldl(
      fun({B, Mods}, Acc) ->
          lists:foldl(
            fun(M, A) ->
                maps:update_with(M, fun(L) -> [B | L] end, [B], A)
            end, Acc, Mods)
      end, #{}, maps:to_list(Ms)),
  {Assign, NGroups} =
    lists:foldl(
      fun(B, {Acc, N}) ->
          MyMods = maps:get(B, Ms),
          Neighbors =
            lists:usort(
              lists:append([maps:get(M, Owners) || M <- MyMods])),
          Used = [G || O <- Neighbors, O =/= B, {ok, G} <- [maps:find(O, Acc)]],
          G = first_free(1, Used),
          {Acc#{B => G}, max(N, G)}
      end, {#{}, 0}, Bases),
  Groups =
    [list_to_atom("g" ++ integer_to_list(I)) || I <- lists:seq(1, NGroups)],
  AtomMap = maps:map(
              fun(_B, I) -> list_to_atom("g" ++ integer_to_list(I)) end,
              Assign),
  {AtomMap, Groups}.

first_free(I, Used) ->
  case lists:member(I, Used) of
    true  -> first_free(I + 1, Used);
    false -> I
  end.

?MEMO(build_inputs,
      begin
        %% Plugin + theories: dune decides what to rebuild
        _ = anvl_lib:exec("dune", ["build"], [{cd, repo_root()}]),
        false
      end).

%% One test/<base>.v -> stage/<base>/*.core (rocq writes to its cwd)
?MEMO(core_extracted, VFile,
      begin
        _ = precondition(build_inputs()),
        Base = filename:rootname(filename:basename(VFile)),
        StageDir = stage_dir(Base),
        Core = filename:join(StageDir, Base ++ ".core"),
        %% the plugin artifact participates: a plugin rebuild must
        %% re-extract even when the .v file is unchanged
        Plugin = filename:join(plugin_dir(), "coreerlang_plugin.cmxs"),
        anvl_lib:newer([VFile, Plugin], Core)
          andalso begin
            %% wipe the staging dir: dropped dependencies of an older
            %% version of the test must not leak into the grouping
            _ = [file:delete(F)
                 || F <- filelib:wildcard(filename:join(StageDir, "*"))],
            ok = filelib:ensure_path(StageDir),
            _ = anvl_lib:exec(
                  "rocq",
                  ["c", "-q",
                   "-I", plugin_dir(),
                   "-Q", theories_dir(), "Erlang",
                   VFile],
                  [{cd, StageDir}]),
            true
          end
      end).

%% All .v files extracted, one condition (and one rocq process) per file
?MEMO(cores_extracted,
      precondition([core_extracted(VFile) || VFile <- v_files()])).

%% Materialize the group apps: one dir per group with an .app.src, the
%% union of its tests' staged .core files, and the tests' eunit
%% suites. Stale group dirs (and leftovers of older layouts) are
%% removed. Content is compared before writing, to avoid mtime churn.
?MEMO(groups_ready,
      begin
        _ = precondition(cores_extracted()),
        {Map, Groups} = group_map(),
        [_ | _] = Groups,
        lists:foreach(
          fun(G) ->
              SrcDir = filename:join(group_dir(G), "src"),
              ok = filelib:ensure_path(SrcDir),
              AppSrc =
                io_lib:format(
                  "~p.~n",
                  [{application, G,
                    [{description, "Extracted Core Erlang modules (group)"},
                     {vsn, "0.1.0"}, {registered, []},
                     {applications, [kernel, stdlib, eunit]}]}]),
              changed_write(filename:join(SrcDir, atom_to_list(G) ++ ".app.src"),
                            unicode:characters_to_binary(AppSrc)),
              Bases = [B || {B, GG} <- maps:to_list(Map), GG =:= G],
              lists:foreach(
                fun(B) ->
                    %% extracted modules
                    lists:foreach(
                      fun(F) ->
                          {ok, Content} = file:read_file(F),
                          changed_write(
                            filename:join(SrcDir, filename:basename(F)),
                            Content)
                      end,
                      filelib:wildcard(filename:join(stage_dir(B), "*.core"))),
                    %% the eunit suite of the test (named <base>_suite:
                    %% the anvl_test eunit discovery skips modules
                    %% called <base>_tests when <base> itself is in the
                    %% same application -- and it always is here, the
                    %% extracted module of the test)
                    Suite = filename:join(suite_src_dir(), B ++ "_suite.erl"),
                    {ok, SuiteSrc} = file:read_file(Suite),
                    changed_write(filename:join(SrcDir, B ++ "_suite.erl"),
                                  SuiteSrc)
                end, Bases)
          end, Groups),
        Keep = ["stage" | [atom_to_list(G) || G <- Groups]],
        _ = [file:del_dir_r(filename:join(extracted_root(), D))
             || D <- filelib:wildcard(filename:join(extracted_root(), "*")),
                not lists:member(filename:basename(D), Keep)],
        true
      end).

%% write only when the content actually changed; true when written
changed_write(File, Content) ->
  case file:read_file(File) of
    {ok, Content} -> false;
    _ ->
      ok = file:write_file(File, Content),
      true
  end.

%% Run the eunit suites of every group app via anvl_test: each suite
%% module is a test case executing on its own node, with a code path
%% built from the group's application closure. (anvl_test:run/2
%% executes the test cases as preconditions of its own and fails the
%% condition when any of them fails.)
?MEMO(all,
      begin
        _ = precondition(groups_ready()),
        {_Map, Groups} = group_map(),
        lists:foldl(
          fun(G, Acc) -> anvl_test:run(default, G) orelse Acc end,
          false, Groups)
      end).
