-module(server).
-export([start_link/0, init/1]).

start_link() ->
  io:format("server start link~n"),
  proc_lib:start_link(?MODULE, init, [self()]).

init(Parent) ->
  io:format("server start~n"),
  {ok, LSock} = gen_tcp:listen(9418, [list, {packet, 0}, {active, false}]),
  proc_lib:init_ack(Parent, {ok, self()}),
  loop(LSock).
    
loop(LSock) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  {ok, List} = gen_tcp:recv(Sock, 0),
  
  io:format("=> ~p~n~n", [List]),
    
  gen_tcp:send(Sock, "ok\n"),
  ok = gen_tcp:close(Sock),
  
  loop(LSock).