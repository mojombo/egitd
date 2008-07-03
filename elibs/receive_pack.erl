-module(receive_pack).
-export([handle/3]).

handle(Sock, _Host, _Header) ->
  ok = gen_tcp:close(Sock).