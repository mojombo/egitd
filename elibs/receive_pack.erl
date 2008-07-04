-module(receive_pack).
-export([handle/3]).

handle(Sock, _Host, _Header) ->
  io:format("receive-pack~n"),
  gen_tcp:send(Sock, "006d\n*********'\n\nYou can't push to git://github.com/user/repo.git\nUse git@github.com:user/repo.git\n\n*********"),
  io:format("sent~n"),
  ok = gen_tcp:close(Sock).