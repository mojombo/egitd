-module(gandalf_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, StartArgs) ->
  io:format("app start ~p~n", [StartArgs]),
  gandalf_sup:start_link(StartArgs).
  
stop(_State) ->
  ok.