%%%-------------------------------------------------------------------
%%% Author  : Tom Preston-Werner
%%%-------------------------------------------------------------------
-module(gandalf_sup).
-behaviour(supervisor).

-export([start/0, start_link/1, init/1]).

start() ->
  spawn(fun() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = [])
  end).
  
start_link(Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).
  
init([]) ->
  {ok, {{one_for_one, 100, 300},
    [{server,
       {server, start_link, []},
       permanent, 10000, worker, [server]}
    ]}}.
