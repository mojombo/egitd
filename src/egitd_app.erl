-module(egitd_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, StartArgs) ->
  egitd_sup:start_link(StartArgs).
  
stop(_State) ->
  ok.