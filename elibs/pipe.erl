-module(pipe).
-export([new/0, write/2, read/2]).

-record(pipe, {pos = 0, size = 0, queue = queue:new()}).

new() ->
  #pipe{}.

write(Bin, Pipe) ->
  #pipe{size = Size, queue = Q} = Pipe,
  Pipe#pipe{size = Size + size(Bin), queue = queue:in(Bin, Q)}.
  
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
      
read_internal(Acc, Num, Q1) ->
  {{value, Bin}, Q2} = queue:out(Q1),
  Size = size(Bin),
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