-module(extr).
-compile(nowarn_export_all).
-compile(export_all).

-include("extr.hrl").

run(F, Args) ->
  io:format("~p", [apply(?MODULE, F, Args)]),
  halt().
