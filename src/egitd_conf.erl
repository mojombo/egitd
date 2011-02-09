-module(egitd_conf).
-export([read_conf/1, convert_path/2, eval_erlang_expr/1, eval_erlang_expr/2,
         concat/2, namespace3/1, md5_namespace3/1, hexmod8/1]).

read_conf(Conf) ->
  {ok, DataBinary} = file:read_file(Conf),
  DataString = binary_to_list(DataBinary),
  Lines = string:tokens(DataString, "\n"),
  lists:foreach(fun(Line) -> parse_conf_line(Line) end, Lines).

convert_path(Host, Path) ->
  [{Host, {Regex, Transform}}] = ets:lookup(db, Host),
	case re:run(Path, Regex, [{capture, all_but_first, list}]) of
    {match, Matches} ->
      Binding = create_binding(Matches),
      % io:format("binding = ~p~n", [Binding]),
      eval_erlang_expr(Transform, Binding);
    nomatch ->
      {error, nomatch}
  end.

%% INTERNAL

parse_conf_line(Line) ->
  [Host, Regex, Transform] = string:tokens(Line, "\t"),
  ets:insert(db, {Host, {Regex, Transform}}).

create_binding(Matches) ->
  Modder = fun(Word, Acc) ->
    {I, Arr} = Acc,
    Mod = {I, Word},
    {I + 1, lists:append(Arr, [Mod])}
  end,
  {_X, Matches2} = lists:foldl(Modder, {1, []}, Matches),
  Binder = fun(Match, B) ->
    {I, Word} = Match,
    Var = list_to_atom("Match" ++ integer_to_list(I)),
    erl_eval:add_binding(Var, Word, B)
  end,
  B1 = erl_eval:new_bindings(),
  lists:foldl(Binder, B1, Matches2).

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
  
md5_namespace3(Name) ->
  [A, B, C, D, E, F | _T] = md5:hex(Name),
  [A, B, $/, C, D, $/, E, F].
  
hexmod8(Name) ->
  <<A:4, _:124>> = erlang:md5(Name),
  integer_to_list(A rem 8).
