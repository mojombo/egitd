-module(receive_pack).
-export([handle/3]).

handle(Sock, _Host, _Header) ->
  gen_tcp:send(Sock, "003aYou cannot push to a git:// address. Use git@ instead."),
  ok = gen_tcp:close(Sock).