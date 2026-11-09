-module(rocq).
%% Mimics the Makefile pipeline for a Rocq test file, from Erlang:
%%   translate(File) : rocq c        -> test_core/<Name>.core
%%   compile(File)   : + erlc        -> test_core/<Name>.beam
%%   load(File)      : + purge/load    the beam into the running node
%%
%% File is a path such as "test/adder.v"; the extracted module is named
%% after the file (see CoreErlang Extraction "test_core/<Name>").
%% Always re-runs every step; assumes rocq and erlc are in PATH.

-export([translate/1, compile/1, load/1]).

-define(PKG_DIR, "build/pkg").
-define(OUT_DIR, "test_core").

translate(File) ->
    Core = core_file(File),
    sh("rocq c -I ~s -R test \"\" ~s", [?PKG_DIR, File]),
    case filelib:is_regular(Core) of
        true -> Core;
        false -> erlang:error({rocq_failed, File})
    end.

compile(File) ->
    Core = translate(File),
    sh("erlc -o ~s ~s", [?OUT_DIR, Core]),
    Beam = beam_file(File),
    case filelib:is_regular(Beam) of
        true -> Beam;
        false -> erlang:error({erlc_failed, Core})
    end.

load(File) ->
    compile(File),
    Module = list_to_atom(base(File)),
    code:add_patha(?OUT_DIR),
    %% delete any old version of the module's code before loading
    code:purge(Module),
    {module, Module} = code:load_abs(filename:join(?OUT_DIR, base(File))),
    Module.

%% --- internals ----------------------------------------------------------

base(File) ->
    filename:rootname(filename:basename(File)).

core_file(File) -> filename:join(?OUT_DIR, base(File) ++ ".core").
beam_file(File) -> filename:join(?OUT_DIR, base(File) ++ ".beam").

sh(Format, Args) ->
    Cmd = io_lib:format(Format, Args),
    io:format("~s~n", [Cmd]),
    Out = os:cmd(Cmd),
    case Out of
        [] -> ok;
        _ -> io:put_chars(Out)
    end.
