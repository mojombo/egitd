-module(server).
-export([start_link/0, init/1]).

start_link() ->
  io:format("server start link~n"),
  proc_lib:start_link(?MODULE, init, [self()]).

init(Parent) ->
  io:format("server start~n"),
  LSock = try_listen(10),
  proc_lib:init_ack(Parent, {ok, self()}),
  loop(LSock).
  
try_listen(0) ->
  io:format("Could not listen on port 9418~n");
try_listen(Times) ->
  Res = gen_tcp:listen(9418, [list, {packet, 0}, {active, false}]),
  case Res of
    {ok, LSock} ->
      io:format("Listening on port 9418~n"),
      LSock;
    {error, Reason} ->
      io:format("Could not listen on port 9418: ~p~n", [Reason]),
      timer:sleep(5000),
      try_listen(Times - 1)
  end.
    
loop(LSock) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  spawn(fun() -> handle_method(Sock) end),
  loop(LSock).
  
handle_method(Sock) ->
  {ok, Binary} = gen_tcp:recv(Sock, 0),
  io:format("=> ~p~n", [Binary]),
  Data = "007c74730d410fcb6603ace96f1dc55ea6196122532d HEAD\0multi_ack thin-pack side-band side-band-64k ofs-delta shallow no-progress\n003e15f0ceeef36e49eb51d6efcc7ed4df7b6a03d14f refs/heads/Bertg\n003e7d1665144a3a975c05f1f43902ddaf084e784dbe refs/heads/debug\n003f60487cc2182dd3f0fbc841d7f45afc61ef18ffc5 refs/heads/debug2\n003d5a3f6be755bbb7deae50065988cbfa1ffa9ab68a refs/heads/dist\n003e7e47fe2bd8d01d481f44d7af0531bd93d3b21c01 refs/heads/local\n003f74730d410fcb6603ace96f1dc55ea6196122532d refs/heads/master\n0000",
  io:format("<= ~p~n", [Data]),
  gen_tcp:send(Sock, Data),
  handle(Sock).

handle(Sock) ->
  io:format("getting more data~n"),
  {ok, Binary} = gen_tcp:recv(Sock, 0),
  io:format("=> ~p~n~n", [Binary]),
  case handle_line(Binary) of
    {data, Data} ->
      io:format("=> ~p~n", [Data]),
      handle(Sock);
    done ->
      io:format("done~n"),
      gen_tcp:send(Sock, "ok\n"),
      ok = gen_tcp:close(Sock)
  end.
  
handle_line("0000009done\n") ->
  done;
handle_line(Line) ->
  {data, Line}.