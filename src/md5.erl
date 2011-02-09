-module(md5).
-export([hex/1]).

hex(String) ->
  transform(binary_to_list(erlang:md5(String))).

transform(L) ->
  lists:flatten([[hex0((I band 16#f0) bsr 4), hex0(I band 16#0f)] || I <- L]).

hex0(10) -> $a;
hex0(11) -> $b;
hex0(12) -> $c;
hex0(13) -> $d;
hex0(14) -> $e;
hex0(15) -> $f;
hex0(I) ->  $0 + I.