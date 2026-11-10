-module(rocq).
%% Standalone helper driving the Core Erlang extraction from Erlang:
%%   translate(File) : rocq c (via open_port)   -> the .core file
%%   compile(File)   : + compiler:file/2        -> <Name>.beam in the CWD
%%   load(File)      : + purge/load               the beam into the node
%%
%% File is a path such as "test/adder.v". The extraction target is read
%% from the .v itself (its CoreErlang Extraction "..." argument); any
%% missing directories along the way are created. The .beam is an
%% intermediate file and always lands in the current working directory.
%% Every step always re-runs, and any failing step crashes (bad exit
%% status, compiler error, load error).

-export([translate/1, compile/1, load/1]).

translate(File) ->
    Target = core_target(File),
    ok = filelib:ensure_dir(Target),          % don't assume dirs exist
    Args = ["c", "-I", pkg_dir(), "-R", filename:dirname(File), ""]
        ++ theories_args(),
    run(rocq, Args ++ [File]),
    Core = Target ++ ".core",
    case filelib:is_regular(Core) of
        true -> Core;
        false -> erlang:error({rocq_produced_no_core, Core})
    end.

compile(File) ->
    Core = translate(File),
    case compile:file(Core, [from_core, {outdir, "."}, return]) of
        {ok, _ModuleName, _WarningsOrCode} -> ok;
        Other -> erlang:error({compile_failed, Core, Other})
    end,
    Beam = base(File) ++ ".beam",             % in the CWD
    case filelib:is_regular(Beam) of
        true -> Beam;
        false -> erlang:error({compile_produced_no_beam, Beam})
    end.

load(File) ->
    compile(File),
    Module = list_to_atom(base(File)),
    %% delete any old version of the module's code before loading
    code:purge(Module),
    {module, Module} = code:load_abs(base(File)),  % crashes on error
    Module.

%% --- internals ----------------------------------------------------------

%% the project root: where this module's beam lives (rocq.erl sits in
%% the root; the staged plugin is at build/pkg below it)
root() ->
    filename:dirname(code:which(rocq)).

pkg_dir() ->
    filename:join(root(), "build/pkg").

%% the support library (theories/Erlang.v), if present
theories_args() ->
    Dir = filename:join(root(), "theories"),
    case filelib:is_dir(Dir) of
        true -> ["-R", Dir, ""];
        false -> []
    end.

base(File) ->
    filename:rootname(filename:basename(File)).

%% run an executable via a port, echoing its output; crash unless it
%% exits with status 0
run(Cmd, Args) ->
    Exe = os:find_executable(atom_to_list(Cmd)),
    Port = erlang:open_port(
             {spawn_executable, Exe},
             [use_stdio, binary, exit_status, stderr_to_stdout,
              {args, Args}]),
    collect(Port, Cmd).

collect(Port, Cmd) ->
    receive
        {Port, {data, Bin}} ->
            io:put_chars(Bin),
            collect(Port, Cmd);
        {Port, eof} ->
            collect(Port, Cmd);
        {Port, {exit_status, 0}} ->
            ok;   %% the port is already closed on exit_status
        {Port, {exit_status, N}} ->
            erlang:error({Cmd, exited, N})
    end.

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
