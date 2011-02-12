-module(egitd_connection).
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
  gen_tcp:send(Sock, Data),
  {noreply, State};
handle_info({_SocketType, Socket, Packet}, #state{socket = Socket, port = Port} = State) when is_port(Port) ->
  port_command(Port, Packet),
  inet:setopts(Socket, [{active, once}]),
  {noreply, State};
handle_info({_SocketType, Socket, <<_Length:4/binary, "git", _:1/binary, Rest/binary>>}, #state{socket = Socket} = State) ->
  [Method, Other] = binary:split(Rest, <<" ">>),
  [Args, <<"host=", Host/binary>>, <<>>] = binary:split(Other, <<0>>, [global]),
  dispatch_method(Method, Host, Args, State);
handle_info({_SocketType, Socket, _Packet}, #state{socket = Socket} = State) ->
  send_error(Socket, "\n*********\n\nInvalid method declaration. Upgrade to the latest git.\n\n*********'"),
  gen_tcp:close(Socket),
  {stop, normal, State};
handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
  {stop, normal, State};
handle_info(_Info, State) ->
  error_logger:info_msg("unhandled info ~p~n", [_Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

dispatch_method(<<"upload-pack">>, Host, Path, #state{socket = Sock} = State) ->
  try
    case conf:convert_path(binary_to_list(Host), binary_to_list(Path)) of
      {ok, FullPath} ->
        case repo_existance(FullPath) of
          false ->
            throw(nomatch);
          RealPath ->
            GitDaemonExportOkFilePath = filename:join([RealPath, "git-daemon-export-ok"]),
            case filelib:is_regular(GitDaemonExportOkFilePath) of
              true ->
                %% all validated, yay
                Port = make_port(Sock, "upload-pack", Host, Path, RealPath),
                inet:setopts(Sock, [{active, once}]),
                {noreply, State#state{port = Port}};
              false ->
                throw({noexport, RealPath})
            end
        end;
      {error, nomatch} ->
        throw(nomatch)
    end
  catch
    throw:nomatch ->
      error_logger:info_msg("no repo match: ~p~n", [Path]),
      send_error(Sock, ["\n*********\n\nNo matching repositories found for git://", Host, Path, ".\n\n*********"]),
      gen_tcp:close(Sock),
      {stop, normal, State};
    throw:{noexport, ThePath} ->
      error_logger:info_msg("permission denied to repo: ~p~n", [ThePath]),
      send_error(Sock, ["\n*********\n\nPermission denied. Repository git://", Host, Path, " is not public.\n\n*********"]),
      gen_tcp:close(Sock),
      {stop, normal, State}
  end;
dispatch_method(<<"receive-pack">>, Host, Path, #state{socket = Sock} = State) ->
  SSHPath = [":", binary:part(Path, {1, byte_size(Path) -1})],
  send_error(Sock, ["\n*********\n\nYou can't push to git://", Host, Path, "\nUse git@", Host, SSHPath, "\n\n*********"]),
  gen_tcp:close(Sock),
  {stop, normal, State};
dispatch_method(Method, _Host, _Args, #state{socket = Sock} = State) ->
  send_error(Sock, ["\n*********\n\nInvalid method declaration: '", Method, "'. Upgrade to the latest git.\n\n*********'"]),
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
  open_port({spawn, Command}, [binary]).

send_error(Socket, Error) ->
  FlatError = list_to_binary(Error),
  ErrorMsg = io_lib:format("~4.16.0B~s", [byte_size(FlatError)+4, FlatError]),
  gen_tcp:send(Socket, ErrorMsg).

