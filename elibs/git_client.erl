-module(git_client).
-behaviour(gen_server).

-record(state, {
    socket,
    port
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


handle_info({Port, {data, Data}}, #state{socket = Sock, port = Port} = State) ->
  %io:format("forwarding data to socket ~p~n", [Data]),
  gen_tcp:send(Sock, Data),
  {noreply, State};
handle_info({_SocketType, Socket, Packet}, #state{socket = Socket, port = Port} = State) when is_port(Port) ->
  %io:format("forwarding data to port ~p~n", [Packet]),
  port_command(Port, Packet),
  inet:setopts(Socket, [{active, once}]),
  {noreply, State};
handle_info({_SocketType, Socket, <<_Length:4/binary, "git", _:1/binary, Rest/binary>>}, #state{socket = Socket} = State) ->
  [Method, Other] = binary:split(Rest, <<" ">>),
  [Args, <<"host=", Host/binary>>, <<>>] = binary:split(Other, <<0>>, [global]),
  io:format("git method ~p; args ~p host ~p~n", [Method, Args, Host]),
  dispatch_method(Method, Host, Args, State);
handle_info({_SocketType, Socket, _Packet}, #state{socket = Socket} = State) ->
  gen_tcp:send(Socket, "Invalid method declaration. Upgrade to the latest git.\n"),
  gen_tcp:close(Socket),
  {stop, normal, State};
handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
  {stop, normal, State};
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

dispatch_method(<<"upload-pack">>, Host, Path, #state{socket = Sock} = State) ->
  try conf:convert_path(binary_to_list(Host), binary_to_list(Path)) of
    {ok, FullPath} ->
      case repo_existance(FullPath) of
        false ->
          throw(nomatch);
        RealPath ->
          GitDaemonExportOkFilePath = filename:join([RealPath, "git-daemon-export-ok"]),
          case filelib:is_regular(GitDaemonExportOkFilePath) of
            true ->
              %% all validated, yay
              io:format("ready to do an upload-pack~n"),
              Port = make_port(Sock, "upload-pack", Host, Path, RealPath),
              inet:setopts(Sock, [{active, once}]),
              {noreply, State#state{port = Port}};
            false ->
              throw({noexport, RealPath})
          end
      end;
    {error, nomatch} ->
      throw(nomatch)
  catch
    throw:nomatch ->
      error_logger:info_msg("no repo match: ~p~n", [Path]),
      gen_tcp:send(Sock, "003b\n*********'\n\nNo matching repositories found.\n\n*********"),
      gen_tcp:close(Sock),
      {stop, normal, State};
    throw:{noexport, RealPath} ->
      error_logger:info_msg("permission denied to repo: ~p~n", [RealPath]),
      gen_tcp:send(Sock, "0048\n*********'\n\nPermission denied. Repository is not public.\n\n*********"),
      gen_tcp:close(Sock),
      {stop, normal, State}
  end;
dispatch_method(<<"receive-pack">>, _Host, _Args, #state{socket = Sock} = State) ->
  %% TODO make this message include the actual repo
  gen_tcp:send(Sock, "006d\n*********'\n\nYou can't push to git://github.com/user/repo.git\nUse git@github.com:user/repo.git\n\n*********"),
  gen_tcp:close(Sock),
  {stop, normal, State};
dispatch_method(_Method, _Host, _Args, #state{socket = Sock} = State) ->
  gen_tcp:send(Sock, "Invalid method declaration. Upgrade to the latest git.\n"),
  gen_tcp:close(Sock),
  {stop, normal, State}.

repo_existance(Path) ->
  case filelib:is_dir(Path) of
    true ->
      Path;
    _ ->
      case filelib:is_dir(Path ++ ".git") of
        true ->
          Path ++ ".git";
        _ ->
          false
      end
  end.

make_port(_Sock, Method, _Host, _Path, FullPath) ->
  Command = lists:flatten(["git ", Method, " ", FullPath]),
  io:format("making port with command ~p~n", [Command]),
  open_port({spawn, Command}, [binary]).

