-module(log).
-export([init_log/1, write/2]).

init_log(Log) ->
  ets:insert(db, {log, Log}).
  
write(Type, Messages) ->
  write_log(ets:lookup(db, log), Type, Messages).
  
write_log([{log, Log}], Type, Messages) ->
  {ok, F} = file:open(Log, [append]),
  Message = string:join(Messages, "\t"),
  Line = io_lib:fwrite("~s\t~s\t~s\n", [timestamp(), Type, Message]),
  file:write(F, Line),
  file:close(F),
  ok;
write_log([], _Type, _Messages) ->
  ok.
  
timestamp() ->
  {{Year, Month, Day}, {Hour, Min, Sec}} = erlang:localtime(),
  io_lib:fwrite("~4B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Year, Month, Day, Hour, Min, Sec]).