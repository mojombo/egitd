-module(egitd_server).
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
  egitd_conf:read_conf(Conf).
  
init_log() ->
  init_log(application:get_env(log)).
init_log({ok, Log}) ->
  log:init_log(Log);
init_log(undefined) ->
  ok.
  
try_listen(0) ->
  error_logger:info_msg("Could not listen on port 9418~n");
try_listen(Times) ->
  Res = gen_tcp:listen(9418, [binary, {packet, 0}, {active, false}]),
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
  {ok, Pid} = egitd_connection:start_link(Sock),
  gen_tcp:controlling_process(Sock, Pid),
  loop(LSock).
  
