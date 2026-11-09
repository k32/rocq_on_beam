-module(rocq).
%% Standalone helper driving the Core Erlang extraction from Erlang:
%%   translate(File) : rocq c        -> the .core file (where the .v says)
%%   compile(File)   : + erlc        -> <Name>.beam in the CWD
%%   load(File)      : + purge/load    the beam into the running node
%%
%% File is a path such as "test/adder.v". The extraction target is read
%% from the .v itself (its CoreErlang Extraction "..." argument); any
%% missing directories along the way are created. The .beam is an
%% intermediate file and always lands in the current working directory.
%% Every step always re-runs; rocq and erlc are assumed in PATH.

-export([translate/1, compile/1, load/1]).


translate(File) ->
    Target = core_target(File),
    ok = filelib:ensure_dir(Target),          % don't assume dirs exist
    sh("rocq c -I ~s -R ~s \"\" ~s",
       [pkg_dir(), filename:dirname(File), File]),
    Core = Target ++ ".core",
    case filelib:is_regular(Core) of
        true -> Core;
        false -> erlang:error({rocq_failed, File})
    end.

compile(File) ->
    Core = translate(File),
    sh("erlc -o . ~s", [Core]),
    Beam = base(File) ++ ".beam",             % in the CWD
    case filelib:is_regular(Beam) of
        true -> Beam;
        false -> erlang:error({erlc_failed, Core})
    end.

load(File) ->
    compile(File),
    Module = list_to_atom(base(File)),
    %% delete any old version of the module's code before loading
    code:purge(Module),
    {module, Module} = code:load_abs(base(File)),
    Module.

%% --- internals ----------------------------------------------------------

%% the project root: where this module's beam lives (rocq.erl sits in
%% the root; the staged plugin is at build/pkg below it)
root() ->
    filename:dirname(code:which(rocq)).

pkg_dir() ->
    filename:join(root(), "build/pkg").

base(File) ->
    filename:rootname(filename:basename(File)).

%% the extraction target as written in the .v:
%% the argument of the first CoreErlang Extraction "..." command
core_target(File) ->
    {ok, Bin} = file:read_file(File),
    find_target(binary_to_list(Bin)).

find_target("CoreErlang" ++ Rest0) ->
    case skip_blanks(Rest0) of
        "Extraction" ++ Rest1 ->
            case skip_blanks(Rest1) of
                [$" | Quoted] ->
                    case string:chr(Quoted, $") of
                        0 -> erlang:error(no_extraction_target);
                        N -> string:substr(Quoted, 1, N - 1)
                    end;
                _ -> erlang:error(no_extraction_target)
            end;
        _ -> find_target(Rest0)
    end;
find_target([_ | Rest]) -> find_target(Rest);
find_target([]) -> erlang:error(no_extraction_command).

skip_blanks([C | Rest]) when C =:= $\s; C =:= $\t; C =:= $\r; C =:= $\n ->
    skip_blanks(Rest);
skip_blanks(S) -> S.

sh(Format, Args) ->
    Cmd = io_lib:format(Format, Args),
    io:format("~s~n", [Cmd]),
    Out = os:cmd(Cmd),
    case Out of
        [] -> ok;
        _ -> io:put_chars(Out)
    end.
