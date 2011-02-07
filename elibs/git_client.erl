-module(git_client).
-behaviour(gen_server).

-record(state, {
    socket
  }).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Socket) ->
  gen_server:start(?MODULE, [Socket], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Socket]) ->
  inet:setopts(Socket, [{active, once}]),
  {ok, #state{socket = Socket}}.

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({_SocketType, Socket, Packet}, #state{socket = Socket} = State) ->
  case Packet of
    <<_:4/binary, "git", _:1/binary, Rest/binary>> ->
      [Command, Args] = binary:split(Rest, <<" ">>),
      io:format("git command ~p; args ~p~n", [Command, Args]),
      {noreply, State};
    _ ->
      gen_tcp:send(Socket, "Invalid method declaration. Upgrade to the latest git.\n"),
      gen_tcp:close(Socket),
      {stop, normal, State}
  end;
handle_info(_Info, State) ->
  io:format("unhandled info ~p~n", [_Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

