-module(conf).
-export([read_conf/1, eval_erlang_expr/1, eval_erlang_expr/2, concat/2, namespace3/1]).

read_conf(Conf) ->
  DB = ets:new(db, [set, named_table]),
  {ok, DataBinary} = file:read_file(Conf),
  DataString = binary_to_list(DataBinary),
  Lines = string:tokens(DataString, "\n"),
  lists:foreach(fun(Line) -> parse_conf_line(Line, DB) end, Lines).

%% INTERNAL

parse_conf_line(Line, DB) ->
  [Host, Regex, Transform] = string:tokens(Line, "\t"),
  ets:insert(DB, {Host, {Regex, Transform}}),
  io:format("~p~n", [ets:lookup(db, Host)]).

eval_erlang_expr(Expr) ->
  eval_erlang_expr(Expr, []).
  
eval_erlang_expr(Expr, Binding) ->
  {ok, Tokens, _} = erl_scan:string(Expr),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  {value, Result, _} = erl_eval:expr(Form, Binding),
  {ok, Result}.

%% CONF FILE API

concat(A, B) ->
  A ++ B.
  
namespace3(Name) ->
  SafeName = Name ++ Name ++ Name,
  [A, B, C | _RestName] = SafeName,
  string:join([[A], [B], [C]], "/").