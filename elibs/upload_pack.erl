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
  send_index_to_client(Port, Sock, Host, Path).
  
% The initial output from git-upload-pack lists the SHA1s of each head.
% data completion is denoted by "0000" on it's own line.
% This is sent back immediately to the client.
send_index_to_client(Port, Sock, Host, Path) ->
  Index = gather_out(Port),
  gen_tcp:send(Sock, Index),
  get_demand_from_client(Port, Sock, Host, Path).
  
% Once the client receives the index data, it will demand that specific
% revisions be packaged and sent back. This demand will be forwarded to
% git-upload-pack.
get_demand_from_client(Port, Sock, Host, Path) ->  
  case gather_demand(Sock) of
    {ok, Demand} ->
      log_initial_clone(Demand, Host, Path),
      port_command(Port, Demand),
      send_pack_to_client(Port, Sock);
    {error, closed} ->
      ok = gen_tcp:close(Sock),
      port_command(Port, "0000"),
      safe_port_close(Port);
    {error, Reason} ->
      error_logger:error_msg("Client closed socket because: ~p~n", [Reason]),
      ok = gen_tcp:close(Sock),
      port_command(Port, "0000"),
      safe_port_close(Port)
  end.
  
% In response to the demand, git-upload-pack will stream out the requested
% pack information. Data completion is denoted by "0000".
send_pack_to_client(Port, Sock) ->
  stream_out(Port, Sock),
  ok = gen_tcp:close(Sock),
  safe_port_close(Port).

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

gather_demand(Sock) ->
  gather_demand(Sock, "").
gather_demand(Sock, DataSoFar) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, Data} ->
      % io:format("data = ~p~n", [Data]),
      TotalData = DataSoFar ++ Data,
      case regexp:first_match(TotalData, "0009done\n$") of
        {match, _Start, _Length} ->
          {ok, TotalData};
        _Else ->
          gather_demand(Sock, TotalData)
      end;
    {error, Reason} ->
      {error, Reason}
  end.

gather_out(Port) ->
  gather_out(Port, [], pipe:new()).
gather_out(Port, DataSoFar, Pipe) ->
  % io:format("gather-out "),
  {ok, Data, P2} = read_chunk(Port, Pipe),
  % io:format("+++~n~p~n+++~n", [Data]),
  TotalData = lists:append(DataSoFar, [Data]),
  case Data =:= <<"0000">> of
    true ->
      list_to_binary(TotalData);
    false ->
      gather_out(Port, TotalData, P2)
  end.
  
stream_out(Port, Sock) ->
  stream_out(Port, Sock, pipe:new()).
  
stream_out(Port, Sock, Pipe) ->
  % io:format("stream out "),
  % io:format("+++~n~p~n+++~n", [Data]),
  {ok, Data, P2} = read_chunk(Port, Pipe),
  gen_tcp:send(Sock, Data),
  case Data =:= <<"0000">> of
    true  -> done;
    false -> stream_out(Port, Sock, P2)
  end.
  
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
      % io:format("readline = ~p~n", [Data]),
      {data, Data};
    Msg ->
      error_logger:error_msg("unknown message ~p~n", [Msg]),
      {error, Msg}
    after 15000 ->
      error_logger:error_msg("timed out waiting for port~n"),
      {error, timeout}
  end.
  
log_initial_clone(Demand, Host, Path) ->
  case regexp:first_match(Demand, "have") of
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