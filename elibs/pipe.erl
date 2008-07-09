% pipe.erl
%
% This module implements a pipe data structure. This pipe implementation is
% designed as a fifo for bytes. You write bytes *to* the pipe and then can
% read those same bytes *from* the pipe. This is useful when dealing with
% chunked data from an external port. All of the chunked data can be written
% to the pipe and then you can read specific numbers of bytes from the pipe.
% This is necessary if you wish to do your own packet length management.
%
% (The MIT License)
% 
% Copyright (c) 2008 Tom Preston-Werner
% 
% Permission is hereby granted, free of charge, to any person obtaining
% a copy of this software and associated documentation files (the
% 'Software'), to deal in the Software without restriction, including
% without limitation the rights to use, copy, modify, merge, publish,
% distribute, sublicense, and/or sell copies of the Software, and to
% permit persons to whom the Software is furnished to do so, subject to
% the following conditions:
% 
% The above copyright notice and this permission notice shall be
% included in all copies or substantial portions of the Software.
% 
% THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND,
% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

-module(pipe).
-export([new/0, write/2, read/2, peek/2, size/1]).

-record(pipe, {pos = 0, size = 0, queue = queue:new()}).

new() ->
  #pipe{}.

write(Bin, Pipe) ->
  #pipe{size = Size, queue = Q} = Pipe,
  {ok, Pipe#pipe{size = Size + erlang:size(Bin), queue = queue:in(Bin, Q)}}.
  
read(Num, Pipe) ->
  #pipe{size = Size, queue = Q1} = Pipe,
  case Num =< Size of
    true ->
      {Acc, Q2} = read_internal([], Num, Q1),
      Bin = list_to_binary(Acc),
      P2 = Pipe#pipe{size = Size - Num, queue = Q2},
      {ok, Bin, P2};
    false ->
      eof
  end.

peek(Num, Pipe) ->
  #pipe{size = Size, queue = Q1} = Pipe,
  case Num =< Size of
    true ->
      {Acc, _} = read_internal([], Num, Q1),
      Bin = list_to_binary(Acc),
      {ok, Bin};
    false ->
      eof
  end.
      
read_internal(Acc, Num, Q1) ->
  {{value, Bin}, Q2} = queue:out(Q1),
  Size = erlang:size(Bin),
  case spaceship(Num, Size) of
    -1 ->
      {B1, B2} = split_binary(Bin, Num),
      Q3 = queue:in_r(B2, Q2),
      Acc2 = lists:append(Acc, [B1]),
      {Acc2, Q3};
    0 ->
      Acc2 = lists:append(Acc, [Bin]),
      {Acc2, Q2};
    1 ->
      Acc2 = lists:append(Acc, [Bin]),
      read_internal(Acc2, Num - Size, Q2)
  end. 
      
size(Pipe) ->
  Pipe#pipe.size.
      
% util

spaceship(A, B) ->
  case A =< B of
    true ->
      case A < B of
        true -> -1;
        false -> 0
      end;
    false -> 1
  end.