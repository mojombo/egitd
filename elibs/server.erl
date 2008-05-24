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
  % get the requested method
  {ok, Binary} = gen_tcp:recv(Sock, 0),
  io:format("=> ~p~n", [Binary]),
  
  % make the port
  Command = "git upload-pack /Users/tom/dev/sandbox/git/god.git",
  Port = open_port({spawn, Command}, []),
  
  % send the output back to client
  Data = gather_out(Port),
  io:format("<= ~p~n", [Data]),
  gen_tcp:send(Sock, Data),
  
  % get the request data from client
  io:format("getting more data~n"),
  {ok, Binary2} = gen_tcp:recv(Sock, 0),
  io:format("=> ~p~n~n", [Binary2]),
  
  % send to port
  port_command(Port, Binary2),
  io:format("Sent pack request to port~n"),
  
  % send the pack
  stream_out(Port, Sock),
  
  % close connection
  gen_tcp:send(Sock, "ok\n"),
  ok = gen_tcp:close(Sock).

gather_out(Port) ->
  gather_out(Port, "").
  
gather_out(Port, DataSoFar) ->
  {data, Data} = readline(Port),
  TotalData = DataSoFar ++ Data,
  case regexp:match(TotalData, "\n0000$") of
    {match, _Start, _Length} ->
      TotalData;
    _Else ->
      gather_out(Port, TotalData)
  end.
  
stream_out(Port, Sock) ->
  {data, Data} = readline(Port),
  gen_tcp:send(Sock, Data),
  io:format("<= ~p~n", [Data]),
  case regexp:match(Data, "0000$") of
    {match, _Start, _Length} ->
      done;
    _Else ->
      stream_out(Port, Sock)
  end.

readline(Port) ->
  receive
    {Port, {data, Data}} ->
      {data, Data};
    Msg ->
      io:format("unknown message ~p~n", [Msg]),
      {error, Msg}
    after 5000 ->
      io:format("timed out waiting for port~n"),
      {error, timeout}
  end.