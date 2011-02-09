-module(egitd).

-export([start/0]).

start() ->
  application:load(egitd),
  application:start(egitd).