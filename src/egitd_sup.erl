%%%-------------------------------------------------------------------
%%% Author  : Tom Preston-Werner
%%%-------------------------------------------------------------------
-module(egitd_sup).
-behaviour(supervisor).

-export([start/0, start_link/1, init/1]).

start() ->
  spawn(fun() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = [])
  end).
  
start_link(Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).
  
init([]) ->
  case application:get_env(pidfile) of
    {ok, Location} ->
      Pid = os:getpid(),
      ok = file:write_file(Location, list_to_binary(Pid));
    undefined -> ok
  end,
  
  {ok, {{one_for_one, 100, 300},
    [{server,
       {egitd_server, start_link, []},
       permanent, 10000, worker, [server]}
    ]}}.
