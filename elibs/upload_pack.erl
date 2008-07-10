-module(upload_pack).
-export([handle/3]).

%****************************************************************************
%
% Entry
%
%****************************************************************************

handle(Sock, Host, Header) ->
  extract_repo_path(Sock, Host, Header).

%****************************************************************************
%
% Main flow
%
%****************************************************************************

% Extract the repo from the header.
extract_repo_path(Sock, Host, Header) ->
  case regexp:match(Header, " /[^\000]+\000") of
    {match, Start, Length} ->
      Path = string:substr(Header, Start + 2, Length - 3),
      convert_path(Sock, Host, Path);
    _Else ->
      invalid
  end.

% Convert the repo path to an absolute path as specified by the config file.
convert_path(Sock, Host, Path) ->
  case conf:convert_path(Host, Path) of
    {ok, FullPath} ->
      repo_existence(Sock, Host, Path, FullPath);
    {error, nomatch} ->
       error_logger:info_msg("no repo match: ~p~n", [Path]),
       gen_tcp:send(Sock, "003b\n*********'\n\nNo matching repositories found.\n\n*********"),
       ok = gen_tcp:close(Sock)
  end.

% Ensure that the repo exists.
repo_existence(Sock, Host, Path, FullPath) ->
  case file_exists(FullPath) of
    true ->
      export_ok(Sock, Host, Path, FullPath);
    false ->
      repo_existence_ext(Sock, Host, Path, FullPath)
  end.
  
% The repo may always be specified without .git on the end
repo_existence_ext(Sock, Host, Path, FullPath) ->
  FullPathExt = FullPath ++ ".git",
  case file_exists(FullPathExt) of
    true ->
      export_ok(Sock, Host, Path, FullPathExt);
    false ->
      error_logger:info_msg("no such repo: ~p~n", [FullPath]),
      gen_tcp:send(Sock, "003b\n*********'\n\nNo matching repositories found.\n\n*********"),
      ok = gen_tcp:close(Sock)
  end.

% Ensure that a 'git-daemon-export-ok' file is present in the repo.
export_ok(Sock, Host, Path, FullPath) ->
  GitDaemonExportOkFilePath = filename:join([FullPath, "git-daemon-export-ok"]),
  case file_exists(GitDaemonExportOkFilePath) of
    true ->
      make_port(Sock, Host, Path, FullPath);
    false ->
      error_logger:info_msg("permission denied to repo: ~p~n", [FullPath]),
      gen_tcp:send(Sock, "0048\n*********'\n\nPermission denied. Repository is not public.\n\n*********"),
      ok = gen_tcp:close(Sock)
  end.

% Create the port to 'git upload-pack'.
make_port(Sock, Host, Path, FullPath) ->
  Command = "git upload-pack " ++ FullPath,
  Port = open_port({spawn, Command}, [binary]),
  send_response_to_client(more, pipe:new(), pipe:new(), Port, Sock, Host, Path, []).
  
% Send a response to the client
send_response_to_client(Status, RequestPipe, ResponsePipe, Port, Sock, Host, Path, FullRequest) ->
  % io:format("send response~n"),
  try
    stream_out(Port, Sock, ResponsePipe)
  catch
    throw:{error, timeout} -> ok
  end,
  case Status of
    more ->
      get_request_from_client(RequestPipe, ResponsePipe, Port, Sock, Host, Path, FullRequest);
    done ->
      ok = gen_tcp:close(Sock),
      safe_port_close(Port)
  end.

% Read a request from a client
get_request_from_client(RequestPipe, ResponsePipe, Port, Sock, Host, Path, FullRequest) ->
  % io:format("get request~n"),
  case gather_request(Sock, RequestPipe) of
    {Status, Request, RequestPipe2} ->
      % io:format("req = ~p~n", [Request]),
      FullRequest2 = [Request | FullRequest],
      case Status of
        more -> ok;
        done -> log_request(string:join(lists:reverse(FullRequest), ""), Host, Path)
      end,
      port_command(Port, Request),
      case pipe:size(RequestPipe2) > 0 of
        true ->
          get_request_from_client(RequestPipe2, ResponsePipe, Port, Sock, Host, Path, FullRequest2);
        false ->
          send_response_to_client(Status, RequestPipe2, ResponsePipe, Port, Sock, Host, Path, FullRequest2)
      end;
    {error, closed} ->
      % io:format("socket closed~n"),
      ok = gen_tcp:close(Sock),
      safe_port_close(Port);
    {error, Reason} ->
      error_logger:error_msg("Client closed socket because: ~p~n", [Reason]),
      ok = gen_tcp:close(Sock),
      safe_port_close(Port)
  end.

%****************************************************************************
%
% Utility functions
%
%****************************************************************************
  
% Safely unlink and close the port. If the port is not open, this is a noop.
safe_port_close(Port) ->
  unlink(Port),
  try port_close(Port)
  catch
    _:_ -> ok
  end.

%****************************************************************************
% gather_request

gather_request(Sock, Pipe) ->
  gather_request(Sock, [], Pipe).
gather_request(Sock, DataSoFar, Pipe) ->
  try
    {ok, Data, P2} = read_chunk_from_socket(Sock, Pipe),
    % io:format("~p~n", [Data]),
    TotalData = lists:append(DataSoFar, [Data]),
    if
      Data =:= <<"0000">> ->
        {more, binary_to_list(list_to_binary(TotalData)), P2};
      Data =:= <<"0009done\n">> ->
        {done, binary_to_list(list_to_binary(TotalData)), P2};
      true ->
        gather_request(Sock, TotalData, P2)
    end
  catch
    _:_ -> {error, closed}
  end.
  
%****************************************************************************
% stream_out

stream_out(Port, Sock, Pipe) ->
  % io:format("stream out "),
  {ok, Data, P2} = read_chunk(Port, Pipe),
  % io:format("~p~n", [Data]),
  gen_tcp:send(Sock, Data),
  case Data =:= <<"0000">> of
    true  -> done;
    false -> stream_out(Port, Sock, P2)
  end.

%****************************************************************************
% read_chunk_from_socket

read_chunk_from_socket(Sock, Pipe) ->
  case pipe:size(Pipe) >= 4 of
    true ->
      {ok, ChunkSizeHex, P2} = pipe:read(4, Pipe),
      % io:format("chunk size hex = ~p~n", [ChunkSizeHex]),
      read_chunk_body_from_socket(ChunkSizeHex, Sock, P2);
    false ->
      {data, Data} = read_from_socket(Sock),
      {ok, P2} = pipe:write(Data, Pipe),
      read_chunk_from_socket(Sock, P2)
  end.

read_chunk_body_from_socket(<<"0000">>, _Sock, Pipe) ->
  {ok, <<"0000">>, Pipe};
  
read_chunk_body_from_socket(ChunkSizeHex, Sock, Pipe) ->
  ChunkSize = convert_chunk_size(ChunkSizeHex),
  % io:format("chunk size = ~p~n", [ChunkSize]),
  case pipe:read(ChunkSize, Pipe) of
    {ok, Bin, P2} ->
      % io:format("chunk body = ~p~n", [Bin]),
      Data = concat_binary([ChunkSizeHex, Bin]),
      {ok, Data, P2};
    eof ->
      % io:format("chunk body eof~n", []),
      {data, Data} = read_from_socket(Sock),
      {ok, P2} = pipe:write(Data, Pipe),
      read_chunk_body_from_socket(ChunkSizeHex, Sock, P2)
  end.
  
read_from_socket(Sock) ->
  case gen_tcp:recv(Sock, 0, 100) of
    {ok, Data} ->
      % io:format("socket = ~p~n", [Data]),
      {data, list_to_binary(Data)};
    {error, Reason} ->
      {error, Reason}
  end.
  
%****************************************************************************
% read_chunk (port)

read_chunk(Port, Pipe) ->
  case pipe:size(Pipe) >= 4 of
    true ->
      {ok, ChunkSizeHex, P2} = pipe:read(4, Pipe),
      % io:format("chunk size hex = ~p~n", [ChunkSizeHex]),
      read_chunk_body(ChunkSizeHex, Port, P2);
    false ->
      {data, Data} = readline(Port),
      {ok, P2} = pipe:write(Data, Pipe),
      read_chunk(Port, P2)
  end.
  
read_chunk_body(<<"0000">>, _Port, Pipe) ->
  {ok, <<"0000">>, Pipe};
  
read_chunk_body(ChunkSizeHex, Port, Pipe) ->
  ChunkSize = convert_chunk_size(ChunkSizeHex),
  % io:format("chunk size = ~p~n", [ChunkSize]),
  case pipe:read(ChunkSize, Pipe) of
    {ok, Bin, P2} ->
      % io:format("chunk body = ~p~n", [Bin]),
      Data = concat_binary([ChunkSizeHex, Bin]),
      {ok, Data, P2};
    eof ->
      % io:format("chunk body eof~n", []),
      {data, Data} = readline(Port),
      {ok, P2} = pipe:write(Data, Pipe),
      read_chunk_body(ChunkSizeHex, Port, P2)
  end.
      
convert_chunk_size(ChunkSizeHex) ->
  {ok, [Size], []} = io_lib:fread("~16u", binary_to_list(ChunkSizeHex)),
  Size - 4.
  
readline(Port) ->
  receive
    {Port, {data, Data}} ->
      % io:format("port = ~p~n", [Data]),
      {data, Data};
    Msg ->
      error_logger:error_msg("unknown message ~p~n", [Msg]),
      {error, Msg}
    after 1000 ->
      error_logger:error_msg("timed out waiting for port~n"),
      throw({error, timeout})
  end.
  
log_request(Request, Host, Path) ->
  case regexp:first_match(Request, "^....want") of
    {match ,_Start, _Length} ->
      log_initial_clone(Request, Host, Path);
    _Else ->
      ok
  end.
  
log_initial_clone(Request, Host, Path) ->
  case regexp:first_match(Request, "have") of
    {match ,_Start, _Length} ->
      ok;
    _Else ->
      ok = log:write("clone", [Host, Path])
  end.
  
file_exists(FullPath) ->
  case file:read_file_info(FullPath) of
    {ok, _Info}      -> true;
    {error, _Reason} -> false
  end.