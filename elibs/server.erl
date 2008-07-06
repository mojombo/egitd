-module(server).
-export([start_link/0, init/1]).

start_link() ->
  proc_lib:start_link(?MODULE, init, [self()]).

init(Parent) ->
  ets:new(db, [set, named_table]),
  read_conf(),
  init_log(),
  log:write("start", ["ok"]),
  LSock = try_listen(10),
  proc_lib:init_ack(Parent, {ok, self()}),
  loop(LSock).
  
read_conf() ->
  {ok, Conf} = application:get_env(conf),
  error_logger:info_msg("Using conf file ~p~n", [Conf]),
  conf:read_conf(Conf).
  
init_log() ->
  init_log(application:get_env(log)).
init_log({ok, Log}) ->
  log:init_log(Log);
init_log(undefined) ->
  ok.
  
try_listen(0) ->
  error_logger:info_msg("Could not listen on port 9418~n");
try_listen(Times) ->
  Res = gen_tcp:listen(9418, [list, {packet, 0}, {active, false}]),
  case Res of
    {ok, LSock} ->
      error_logger:info_msg("Listening on port 9418~n"),
      LSock;
    {error, Reason} ->
      error_logger:info_msg("Could not listen on port 9418: ~p~n", [Reason]),
      timer:sleep(5000),
      try_listen(Times - 1)
  end.
    
loop(LSock) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  spawn(fun() -> handle_method(Sock) end),
  loop(LSock).
  
handle_method(Sock) ->
  % get the requested host and method
  case gen_tcp:recv(Sock, 0) of
    {ok, Header} ->
      % io:format("header = ~p~n", [Header]),
      {ok, Host} = extract_host(Header),
      Method = extract_method_name(Header),
  
      % dispatch
      handle_method_dispatch(Method, Sock, Host, Header);
    {error, closed} ->
      ok = gen_tcp:close(Sock)
  end.
  
handle_method_dispatch({ok, "upload-pack"}, Sock, Host, Header) ->
  upload_pack:handle(Sock, Host, Header);
handle_method_dispatch({ok, "receive-pack"}, Sock, Host, Header) ->
  receive_pack:handle(Sock, Host, Header);
handle_method_dispatch(invalid, Sock, _Host, _Header) ->
  gen_tcp:send(Sock, "Invalid method declaration. Upgrade to the latest git.\n"),
  ok = gen_tcp:close(Sock).
  
extract_method_name(Header) ->
  case regexp:match(Header, "....git[ -][a-z\-]+ ") of
    {match, Start, Length} ->
      {ok, string:substr(Header, Start + 8, Length - 9)};
    _Else ->
      invalid
  end.
  
extract_host(Header) ->
  case regexp:match(string:to_lower(Header), "\000host=[^\000]+\000") of
    {match, Start, Length} ->
      {ok, string:substr(Header, Start + 6, Length - 7)};
    _Else ->
      {ok, "invalid"}
  end.