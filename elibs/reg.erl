%% re4 => Upgrade to needed state, checking speed as we go
%%      0) fixed interval code and copying
%%      1) upgrade to next_match_XXX (+ slight improvement)
%%      2) put pos+char in global state arg (no noticable difference)
%%      3) get look-ahead character for proper eol (- 5-10% slower)

-module(reg).

-export([parse/1,match/2,first_match/2,matches/2,sub/3,gsub/3,split/2]).
-export([smatch/2,first_smatch/2]).
-export([tt/2,loadf/1]).

-import(string, [substr/2,substr/3]).
-import(lists, [reverse/1,reverse/2,last/1,duplicate/2,seq/2]).
-import(lists, [member/2,sort/1,keysearch/3,keysort/2,keydelete/3]).
-import(lists, [map/2,foldl/3]).
-import(ordsets, [is_element/2,add_element/2,union/2,subtract/2]).

%%-compile([export_all]).

%%-define(TP(F,As), io:fwrite(F, As)).
%%-define(TP(F,As), begin {F,As}, ok end).
-define(TP(F,As), ok).

%% NFA states
%% State type defines type of transition from the state.
%% N.B. all types have the id in the same field and all types except
%% estate have the next state pointer in the same field. This is an
%% invariant and is used in the code!

-record(cstate, {id,c,s}).			%Character state
-record(nstate, {id,cc,s}).			%Character class state
-record(estate, {id,s1,s2}).			%Epsilon state
-record(lstate, {id,n,s}).			%Parentheses states
-record(rstate, {id,n,s}).
-record(pstate, {id,t,s}).			%Position states

%% This is the regular expression grammar used. It is equivalent to the
%% one used in AWK, except that we allow ^ $ to be used anywhere and fail
%% in the matching.
%%
%% reg -> reg1 : '$1'.
%% reg1 -> reg1 "|" reg2 : {'or','$1','$2'}.
%% reg1 -> reg2 : '$1'.
%% reg2 -> reg2 reg3 : {concat,'$1','$2'}.
%% reg2 -> reg3 : '$1'.
%% reg3 -> reg3 "*" : {kclosure,'$1'}.
%% reg3 -> reg3 "+" : {pclosure,'$1'}.
%% reg3 -> reg3 "?" : {optional,'$1'}.
%% reg3 -> reg3 "{" [Min],[Max] "}" : {closure_range, Num, '$1'} see below
%% reg3 -> reg4 : '$1'.
%% reg4 -> "(" reg ")" : '$2'.
%% reg4 -> "\\" char : '$2'.
%% reg4 -> "^" : bos.
%% reg4 -> "$" : eos.
%% reg4 -> "." : char.
%% reg4 -> "[" class "]" : {char_class,char_class('$2')}
%% reg4 -> "[" "^" class "]" : {comp_class,char_class('$3')}
%% reg4 -> "\"" chars "\"" : char_string('$2')
%% reg4 -> char : '$1'.
%% reg4 -> empty : epsilon.
%%  The grammar of the current regular expressions. The actual parser
%%  is a recursive descent implementation of the grammar.

%% reg(String, NFA, NextState, SubCount) ->
%%      {Frame,NFA,NewNextState,NewSubCount,RestString}.
%% Frame = {BegState,[EndState]}.

reg(Cs0) ->
    {F,Nfa0,N0,Sc,Cs1} = reg(Cs0, [], 1, 1),
    Nfa1 = [#cstate{id=N0,c=done}|Nfa0],
    {{Start,_},Nfa2,_} = concat(F, {N0,[N0]}, Nfa1, N0),
    {ok,{list_to_tuple(keysort(#nstate.id, Nfa2)),Start,Sc-1},Cs1}.

reg(Cs, Nfa, N, Sc) -> reg1(Cs, Nfa, N, Sc).

%% reg1 -> reg2 reg1'
%% reg1' -> "|" reg2 reg1'
%% reg1' -> empty

reg1(Cs0, Nfa0, N0, Sc0) ->
    {F,Nfa1,N1,Sc1,Cs1} = reg2(Cs0, Nfa0, N0, Sc0),
    reg1p(Cs1, F, Nfa1, N1, Sc1).

reg1p([$||Cs0], Lf, Nfa0, N0, Sc0) ->
    {Rf,Nfa1,N1,Sc1,Cs1} = reg2(Cs0, Nfa0, N0, Sc0),
    {F,Nfa2,N2} = alt(Lf, Rf, Nfa1, N1),
    reg1p(Cs1, F, Nfa2, N2, Sc1);
reg1p(Cs, F, Nfa, N, Sc) -> {F,Nfa,N,Sc,Cs}.

%% reg2 -> reg3 reg2'
%% reg2' -> reg3
%% reg2' -> empty

reg2(S0, Nfa0, N0, Sc0) ->
    {F,Nfa1,N1,Sc1,S1} = reg3(S0, Nfa0, N0, Sc0),
    reg2p(S1, F, Nfa1, N1, Sc1).

reg2p([C|_]=Cs0, Lf, Nfa0, N0, Sc0) when C /= $|, C /= $) ->
    {Rf,Nfa1,N1,Sc1,Cs1} = reg3(Cs0, Nfa0, N0, Sc0),
    {F,Nfa2,N2} = concat(Lf, Rf, Nfa1, N1),
    reg2p(Cs1, F, Nfa2, N2, Sc1);
reg2p(Cs, F, Nfa, N, Sc) -> {F,Nfa,N,Sc,Cs}.

%% reg3 -> reg4 reg3'
%% reg3' -> "*" reg3'
%% reg3' -> "+" reg3'
%% reg3' -> "?" reg3'
%% reg3' -> "{" [Min],[Max] "}" reg3'
%% reg3' -> empty

reg3(Cs0, Nfa0, N0, Sc0) ->
    {F,Nfa1,N1,Sc1,Cs1} = reg4(Cs0, Nfa0, N0, Sc0),
    reg3p(Cs1, F, Nfa1, N1, Sc1).

reg3p([$*|Cs], Lf, Nfa0, N0, Sc) ->
    {F,Nfa1,N1} = kclosure(Lf, Nfa0, N0),
    reg3p(Cs, F, Nfa1, N1, Sc);
reg3p([$+|Cs], Lf, Nfa0, N0, Sc) ->
    {F,Nfa1,N1} = pclosure(Lf, Nfa0, N0),
    reg3p(Cs, F, Nfa1, N1, Sc);
reg3p([$?|Cs], Lf, Nfa0, N0, Sc) ->
    {F,Nfa1,N1} = optional(Lf, Nfa0, N0),
    reg3p(Cs, F, Nfa1, N1, Sc);
reg3p([${|Cs0], Lf, Nfa0, N0, Sc) ->			% $}
    %% Have many special case so as not to create unnecessary new states.
    case interval_range(Cs0) of
	{0,0,[$}|Cs1]} ->			%This is a null op!
	    %% The created states have been created but never be refenced!
	    {Nfa1,N1} = delete(Lf, Nfa0, N0),
 	    reg3p(Cs1, epsilon, Nfa1, N1, Sc);
	{0,Max,[$}|Cs1]} when is_integer(Max) ->
	    {F,Nfa1,N1} = optional(Max, Lf, Nfa0, N0),
	    %%?TP("I2: ~w x ~w x ~w\nI2 => ~w x ~w\n", [Lf,Max,Nfa0,F,Nfa1]),
	    reg3p(Cs1, F, Nfa1, N1, Sc);
	{0,none,[$}|Cs1]} ->			%This is a null op!
	    %% The created states have been created but never be refenced!
	    {Nfa1,N1} = delete(Lf, Nfa0, N0),
 	    reg3p(Cs1, epsilon, Nfa1, N1, Sc);
 	{0,any,[$}|Cs1]} ->
	    {F1,Nfa1,N1} = kclosure(Lf, Nfa0, N0),
 	    reg3p(Cs1, F1, Nfa1, N1, Sc);
	{Min,Min,[$}|Cs1]} when is_integer(Min) ->
	    {F,Nfa1,N1} = copy_concat(Min, Lf, Nfa0, N0),
	    reg3p(Cs1, F, Nfa1, N1, Sc);
 	{Min,Max,[$}|Cs1]} when is_integer(Min), is_integer(Max), Max >= Min ->
	    {Fc,Nfa1,N1} = copy(Lf, Nfa0, N0),	%Make copy first!
	    {F0,Nfa2,N2} = copy_concat(Min, Lf, Nfa1, N1),
	    {F1,Nfa3,N3} = optional(Max-Min, Fc, Nfa2, N2),
	    {F2,Nfa4,N4} = concat(F0, F1, Nfa3, N3),
	    reg3p(Cs1, F2, Nfa4, N4, Sc);
	{Min,none,[$}|Cs1]} when is_integer(Min) ->
	    {F,Nfa1,N1} = copy_concat(Min, Lf, Nfa0, N0),
	    reg3p(Cs1, F, Nfa1, N1, Sc);
 	{Min,any,[$}|Cs1]} when is_integer(Min) ->
	    {Fc,Nfa1,N1} = copy(Lf, Nfa0, N0),	%Make copy first!
	    {F0,Nfa2,N2} = copy_concat(Min, Lf, Nfa1, N1),
	    {F1,Nfa3,N3} = kclosure(Fc, Nfa2, N2),
	    {F2,Nfa4,N4} = concat(F0, F1, Nfa3, N3),
 	    reg3p(Cs1, F2, Nfa4, N4, Sc);
	{_N,_M,_Cs1} ->				%Catches none,none as well
	    parse_error({interval_range,[${|Cs0]})
    end;
reg3p(Cs, Lf, Nfa, N, Sc) -> {Lf,Nfa,N,Sc,Cs}.

reg4([$(,$?,$:|Cs0], Nfa0, N0, Sc0) ->		% $) A little PERLism!
    case reg(Cs0, Nfa0, N0, Sc0) of
	{R,Nfa1,N1,Sc1,[$)|Cs1]} ->
	    {R,Nfa1,N1,Sc1,Cs1};
	{_,_,_,_,_} -> parse_error({unterminated,"(?:"})
    end;
reg4([$(|Cs0], Nfa0, N0, Sc0) ->		% $)
    {Lf,Nfa1,N1} = lparen(Sc0, Nfa0, N0),
    case reg(Cs0, Nfa1, N1, Sc0+1) of
	{R,Nfa2,N2,Sc2,[$)|Cs1]} ->
	    {Sf,Nfa3,N3} = rparen(Sc0, R, Lf, Nfa2, N2),
	    {Sf,Nfa3,N3,Sc2,Cs1};
	{_,_,_,_,_} -> parse_error({unterminated,"("})
    end;
reg4([$^|Cs], Nfa0, N0, Sc) ->
    {F,Nfa1,N1} = pstate(bos, Nfa0, N0),
    {F,Nfa1,N1,Sc,Cs};
reg4([$$|Cs], Nfa0, N0, Sc) ->
    {F,Nfa1,N1} = pstate(eos, Nfa0, N0),
    {F,Nfa1,N1,Sc,Cs};
reg4([$.|Cs], Nfa0, N0, Sc) ->
    {F,Nfa1,N1} = nstate([{0,9},{11,maxchar}], Nfa0, N0),
    {F,Nfa1,N1,Sc,Cs};
reg4([$[,$^|Cs0], Nfa0, N0, Sc) ->
    case comp_class(Cs0) of
	{Cc,[$]|Cs1]} ->
 	    {F,Nfa1,N1} = nstate(Cc, Nfa0, N0),
	    {F,Nfa1,N1,Sc,Cs1};
	{_,_} -> parse_error({unterminated,"["})
    end;
reg4([$[|Cs0], Nfa0, N0, Sc) ->
    case char_class(Cs0) of
	{Cc,[$]|Cs1]} ->
 	    {F,Nfa1,N1} = nstate(Cc, Nfa0, N0),
	    {F,Nfa1,N1,Sc,Cs1};
	{_,_} -> parse_error({unterminated,"["})
    end;
reg4([C0|Cs0], Nfa0, N0, Sc) when
  is_integer(C0), C0 /= $*, C0 /= $+, C0 /= $?, C0 /= $], C0 /= $), C0 /= $} ->
    %% Handle \ quoted characters as well, at least those we see.
    {C1,Cs1} = char(C0, Cs0),			%Get the extended char
    {F,Nfa1,N1} = cstate(C1, Nfa0, N0),
    {F,Nfa1,N1,Sc,Cs1};
reg4([$)|_]=Cs, Nfa, N, Sc) -> {epsilon,Nfa,N,Sc,Cs};
reg4([C|_], _, _, _) -> parse_error({illegal,[C]});
reg4([], Nfa, N, Sc) ->
    ?TP("reg4: ~w\n", [{[],Nfa,N,Sc}]),
    {epsilon,Nfa,N,Sc,[]}.

%%% Is {N,[]} an epsilon state? Is it safe???????

lparen(Sc, Nfa0, N) ->
    Nfa1 = [#lstate{id=N,n=Sc}|Nfa0],
    {{N,[N]},Nfa1,N+1}.

rparen(Sc, epsilon, {Lb,Les}, Nfa0, N) ->
    Nfa1 = patch(Nfa0, Les, N),
    Nfa2 = [#rstate{id=N,n=Sc}|Nfa1],
    {{Lb,[N]},Nfa2,N+1};
rparen(Sc, {B,Es}, {Lb,Les}, Nfa0, N) ->
    Nfa1 = patch(Nfa0, Les, B),
    Nfa2 = [#rstate{id=N,n=Sc}|Nfa1],
    Nfa3 = patch(Nfa2, Es, N),
    {{Lb,[N]},Nfa3,N+1}.

kclosure(epsilon, Nfa, N) -> {epsilon,Nfa,N};
kclosure({B,Es}, Nfa0, N) ->
    Nfa1 = [#estate{id=N,s1=B,s2=none}|Nfa0],
    {{N,[N]},patch(Nfa1, Es, N),N+1}.

pclosure(epsilon, Nfa, N) -> {epsilon,Nfa,N};
pclosure({B,Es}, Nfa0, N) ->
    Nfa1 = [#estate{id=N,s1=B,s2=none}|Nfa0],
    {{B,[N]},patch(Nfa1, Es, N),N+1}.

optional(epsilon, Nfa, N) -> {epsilon,Nfa,N};
optional({B,Es}, Nfa0, N) ->
    Nfa1 = [#estate{id=N,s1=B,s2=none}|Nfa0],
    {{N,Es ++ [N]},Nfa1,N+1}.

cstate(C, Nfa0, N) ->
    Nfa1 = [#cstate{id=N,c=C}|Nfa0],
    {{N,[N]},Nfa1,N+1}.

nstate(Cc, Nfa0, N) ->
    Nfa1 = [#nstate{id=N,cc=Cc}|Nfa0],
    {{N,[N]},Nfa1,N+1}.

pstate(Type, Nfa0, N) ->
    Nfa1 = [#pstate{id=N,t=Type}|Nfa0],
    {{N,[N]},Nfa1,N+1}.

concat(epsilon, F2, Nfa, N) -> {F2,Nfa,N};
concat(F1, epsilon, Nfa, N) -> {F1,Nfa,N};
concat({B1,Es1}, {B2,Es2}, Nfa0, N) ->
    Nfa1 = patch(Nfa0, Es1, B2),
    {{B1,Es2},Nfa1,N}.

alt(epsilon, {B2,E2}, Nfa0, N) ->
    Nfa1 = [#estate{id=N,s1=none,s2=B2}|Nfa0],
    {{N,[N|E2]},Nfa1,N+1};
alt({B1,E1}, epsilon, Nfa0, N) ->
    Nfa1 = [#estate{id=N,s1=B1,s2=none}|Nfa0],
    {{N,E1 ++ [N]},Nfa1,N+1};
alt({B1,E1}, {B2,E2}, Nfa0, N) ->
    Nfa1 = [#estate{id=N,s1=B1,s2=B2}|Nfa0],
    {{N,E1 ++ E2},Nfa1,N+1}.

%% optional(Count, Frame, Nfa, NextFree) -> {Frame,Nfa,NextFree}.
%%  M x F => (...((F?)F)?...F)? Is this better than F?F?...F? ?
%%  Original states will be destructively included in copy.
%%  If Count == 0 then return epsilon.

optional(M, F, Nfa0, N0) when M > 1 ->
    {F1,Nfa1,N1} = copy(F, Nfa0, N0),
    {F2,Nfa2,N2} = optional(M-1, F, Nfa1, N1),
    {F3,Nfa3,N3} = concat(F1, F2, Nfa2, N2),
    optional(F3, Nfa3, N3);
optional(1, F, Nfa, N) -> optional(F, Nfa, N);
optional(0, _, Nfa, N) -> {epsilon,Nfa,N}.

%% copy_concat(Count, Frame, Nfa, NextFree) -> {Frame,Nfa,NextFree}.
%%  Make Count copies of sub-expression in Frame concated together.
%%  Original states will be destructively included in copy.
%%  If Count == 0 then return epsilon.

copy_concat(M, F0, Nfa0, N0) when M > 1 ->
    {F1,Nfa1,N1} = copy(F0, Nfa0, N0),
    {F2,Nfa2,N2} = copy_concat(M-1, F0, Nfa1, N1),
    concat(F1, F2, Nfa2, N2);
copy_concat(1, F, Nfa, N) -> {F,Nfa,N};
copy_concat(0, _, Nfa, N) -> {epsilon,Nfa,N}.

%% copy(Frame, Nfa, NextFree) -> {Frame,Nfa,NextFree}.
%%  Making a copy of a sub expression is a bit of a pain. We
%%  recursivley descend from the start through the graph building new
%%  states as we go back up. We assume that the graph to be copied has
%%  not been already prepended to another set of states as the
%%  termination condition is a non-numeric "next state".

copy({B0,Es}, Nfa0, N0) ->
    {B1,Nfa1,N1,D} = copy(B0, Nfa0, N0, []),
    %% Build a new list of end states from the new copies.
    Es1 = map(fun (E) -> {value,{E,E1}} = keysearch(E, 1, D), E1 end, Es),
    {{B1,Es1},Nfa1,N1}.

copy(B, Nfa0, N0, D0) when is_integer(B) ->
    case keysearch(B, 1, D0) of
	{value,{B,Rep}} -> {Rep,Nfa0, N0, D0};
	false ->
	    case keysearch(B, #cstate.id, Nfa0) of
		{value,#estate{s1=S0,s2=T0}=St} ->
		    {S1,Nfa1,N1,D1} = copy(S0, Nfa0, N0, D0),
		    {T1,Nfa2,N2,D2} = copy(T0, Nfa1, N1, D1),
		    Nfa3 = [St#estate{id=N2,s1=S1,s2=T1}|Nfa2],
		    {N2,Nfa3,N2+1,[{B,N2}|D2]};
		{value,St0} ->
		    %% All other state types have the next state in
		    %% the same place.
		    S0 = element(#cstate.s, St0),
		    {S1,Nfa1,N1,D1} = copy(S0, Nfa0, N0, D0),
		    St1 = setelement(#cstate.id, St0, N1), %{id=N1,s=S1}
		    St2 = setelement(#cstate.s, St1, S1),
		    {N1,[St2|Nfa1],N1+1,[{B,N1}|D1]}
	    end
%% 		{value,#cstate{s=S0}=St} ->
%% 		    {S1,Nfa1,N1,D1} = copy(S0, Nfa0, N0, D0),
%% 		    Nfa2 = [St#cstate{id=N1,s=S1}|Nfa1],
%% 		    {N1,Nfa2,N1+1,[{B,N1}|D1]};
    end;
copy(B, Nfa, N, D) -> {B,Nfa,N,D}.

%% delete(Frame, Nfa, NextFree) -> {Nfa,NextFree}.
%%  Delete all the states in a frame if possible.
%%  This is hairy. Can ony delete from the highest element as holes
%%  not allowed.

delete({B,_}, Nfa, N0) ->
    Ss0 = span_states(B, Nfa, []),		%All states in this frame
    Ss1 = reverse(sort(Ss0)),			%Reverse order
    delete1(Ss1, Nfa, N0).			%Remove until not highest.

delete1([S|Ss], Nfa, N) ->
    if S == N-1 ->				%Highest id element.
	    delete1(Ss, keydelete(S, #cstate.id, Nfa), N-1);
       true -> {Nfa,N}				%No need to go on
    end;
delete1([], Nfa, N) -> {Nfa,N}.

span_states(B, Nfa, Seen) when is_integer(B) ->
    case member(B, Seen) of
	true -> Seen;
	false ->
	    case keysearch(B, #cstate.id, Nfa) of
		{value,#estate{s1=S,s2=T}} ->
		    span_states(T, Nfa, span_states(S, Nfa, [B|Seen]));
		{value,St} ->
		    %% All other state types have the next state in
		    %% the same place.
		    span_states(element(#cstate.s, St), Nfa, [B|Seen])
	    end
    end;
span_states(_, _, Seen) -> Seen.

%% patch(NFA, EndStates, Beginning) -> NFA.
%%  Patch Endstates so they all point to Beginning.

patch(Nfa, Es, B) ->
    lists:foldl(fun (E, Nfa0) -> patch1(Nfa0, E, B) end, Nfa, Es).

patch1([#cstate{id=E}=Nst|Nfa], E, B) ->
    [Nst#cstate{s=B}|Nfa];
patch1([#nstate{id=E}=Nst|Nfa], E, B) ->
    [Nst#nstate{s=B}|Nfa];
%% Patch empty slot of estate, assume there is only 1 empty.
patch1([#estate{id=E,s1=none}=Nst|Nfa], E, B) ->
    [Nst#estate{s1=B}|Nfa];
patch1([#estate{id=E,s2=none}=Nst|Nfa], E, B) ->
    [Nst#estate{s2=B}|Nfa];
patch1([#lstate{id=E}=Nst|Nfa], E, B) ->
    [Nst#lstate{s=B}|Nfa];
patch1([#rstate{id=E}=Nst|Nfa], E, B) ->
    [Nst#rstate{s=B}|Nfa];
patch1([#pstate{id=E}=Nst|Nfa], E, B) ->
    [Nst#pstate{s=B}|Nfa];
patch1([Nst|Nfa], E, B) ->
    [Nst|patch1(Nfa, E, B)].

parse_error(E) -> throw({error,E}).

char($\\, [O1,O2,O3|S]) when
  O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    {(O1*8 + O2)*8 + O3 - 73*$0,S};
char($\\, [C|S]) -> {escape_char(C),S};
char($\\, []) -> parse_error({unterminated,"\\"});
char(C, S) -> {C,S}.

escape_char($n) -> $\n;				%\n = LF
escape_char($r) -> $\r;				%\r = CR
escape_char($t) -> $\t;				%\t = TAB
escape_char($v) -> $\v;				%\v = VT
escape_char($b) -> $\b;				%\b = BS
escape_char($f) -> $\f;				%\f = FF
escape_char($e) -> $\e;				%\e = ESC
escape_char($s) -> $\s;				%\s = SPACE
escape_char($d) -> $\d;				%\d = DEL
escape_char(C) -> C.

char_class([$]|S0]) ->
    {Cc,S1} = char_class(S0, [$]]),
    {pack_cc(Cc),S1};
char_class(S0) ->
    {Cc,S1} = char_class(S0, []),
    {pack_cc(Cc),S1}.

comp_class(Cs0) ->
    {Cc,Cs1} = char_class(Cs0),
    {comp_class(Cc, 0),Cs1}.

comp_class([{C1,C2}|Crs], Last) ->
    [{Last,C1-1}|comp_class(Crs, C2+1)];
comp_class([C|Crs], Last) when Last == C-1 ->
    [Last|comp_class(Crs, C+1)];
comp_class([C|Crs], Last) when is_integer(C) ->
    [{Last,C-1}|comp_class(Crs, C+1)];
comp_class([], Last) -> [{Last,maxchar}].

%% pack_cc(CharClass) -> CharClass
%%  Pack and optimise a character class specification (bracket
%%  expression). First sort it and then compact it.

pack_cc(Cc0) ->
    %% First sort the list ...
    Cc1 = lists:usort(fun ({Cf1,_}, {Cf2,_}) -> Cf1 < Cf2;
			  ({Cf1,_}, C) -> Cf1 < C;
			  (C, {Cf,_}) -> C < Cf;
			  (C1, C2) -> C1 =< C2
		      end, Cc0),
    %% ... then compact it.
    pack_cc1(Cc1).

pack_cc1([{Cf1,Cl1},{Cf2,Cl2}|Cc]) when Cl1 >= Cf2, Cl1 =< Cl2 ->
    %% Cf1       Cl1
    %%     Cf2       Cl2
    pack_cc1([{Cf1,Cl2}|Cc]);
pack_cc1([{Cf1,Cl1},{Cf2,Cl2}|Cc]) when Cl1 >= Cf2, Cl1 >= Cl2 ->
    %% Cf1       Cl1
    %%     Cf2 Cl2
    pack_cc1([{Cf1,Cl1}|Cc]);
pack_cc1([{Cf1,Cl1},{Cf2,Cl2}|Cc]) when Cl1+1 == Cf2 ->
    %% Cf1    Cl1
    %%           Cf2   Cl2
    pack_cc1([{Cf1,Cl2}|Cc]);
pack_cc1([{Cf,Cl},C|Cc]) when Cl >= C -> pack_cc1([{Cf,Cl}|Cc]);
pack_cc1([{Cf,Cl},C|Cc]) when Cl+1 == C -> pack_cc1([{Cf,C}|Cc]);
pack_cc1([C,{Cf,Cl}|Cc]) when C == Cf-1 -> pack_cc1([{C,Cl}|Cc]);
pack_cc1([C1,C2|Cc]) when C1+1 == C2 -> pack_cc1([{C1,C2}|Cc]);
pack_cc1([C|Cc]) -> [C|pack_cc1(Cc)];
pack_cc1([]) -> [].

char_class("[:" ++ S0, Cc0) ->			%Start of POSIX char class
    case posix_cc(S0, Cc0) of
	{Cc1,":]" ++ S1} -> char_class(S1, Cc1);
	{_,_S1} -> parse_error({posix_cc,"[:" ++ S0})
    end;
char_class([C1|S0], Cc) when C1 /= $] ->
    case char(C1, S0) of
	{Cf,[$-,C2|S1]} when C2 /= $] ->
	    case char(C2, S1) of
		{Cl,S2} when Cf < Cl -> char_class(S2, [{Cf,Cl}|Cc]); 
		{_Cl,_S2} -> parse_error({char_class,[C1|S0]})
	    end;
	{C,S1} -> char_class(S1, [C|Cc])
    end;
char_class(S, Cc) -> {Cc,S}.

%% posix_cc(String, CharClass) -> {NewCharClass,RestString}.
%%  Handle POSIX character classes, use Latin-1 character set.

posix_cc("alnum" ++ S, Cc) ->
    {[{$0,$9},{$A,$Z},{192,214},{216,223},{$a,$z},{224,246},{248,255}|Cc],S};
posix_cc("alpha" ++ S, Cc) ->
    {[{$A,$Z},{192,214},{216,223},{$a,$z},{224,246},{248,255}|Cc],S};
posix_cc("blank" ++ S, Cc) -> {[$\s,$\t,160|Cc],S};
posix_cc("cntrl" ++ S, Cc) -> {[{0,31},{127,159}|Cc],S};
posix_cc("digit" ++ S, Cc) -> {[{$0,$9}|Cc],S};
posix_cc("graph" ++ S, Cc) -> {[{33,126},{161,255}|Cc],S};
posix_cc("lower" ++ S, Cc) -> {[{$a,$z},{224,246},{248,255}|Cc],S};
posix_cc("print" ++ S, Cc) -> {[{32,126},{160,255}|Cc],S};
posix_cc("punct" ++ S, Cc) -> {[{$!,$/},{$:,$?},{${,$~},{161,191}|Cc],S};
posix_cc("space" ++ S, Cc) -> {[$\s,$\t,$\f,$\r,$\v,160|Cc],S};
posix_cc("upper" ++ S, Cc) -> {[{$A,$Z},{192,214},{216,223}|Cc],S};
posix_cc("xdigit" ++ S, Cc) -> {[{$a,$f},{$A,$F},{$0,$9}|Cc],S};
posix_cc(S, _Cc) -> parse_error({posix_cc,"[:" ++ S}).

interval_range(Cs0) ->
    case number(Cs0) of
	{none,Cs1} -> {none,none,Cs1};
	{N,[$,|Cs1]} ->
	    case number(Cs1) of
		{none,Cs2} -> {N,any,Cs2};
		{M,Cs2} -> {N,M,Cs2}
	    end;
	{N,Cs1} -> {N,none,Cs1}
    end.

number([C|Cs]) when C >= $0, C =< $9 ->
    number(Cs, C - $0);
number(Cs) -> {none,Cs}.

number([C|Cs], Acc) when C >= $0, C =< $9 ->
    number(Cs, 10*Acc + (C - $0));
number(Cs, Acc) -> {Acc,Cs}.

%% The interface functions.

parse(Cs) ->
    case catch reg(Cs) of
	{ok,R,[]} -> {ok,{nfa,R}};
	{ok,_R,[C|_]} -> {error,{illegal,[C]}};
	{error,E} -> {error,E}
    end.

%% match(String, RegExp) -> {match,Start,Length} | nomatch | {error,E}.
%%  Find the longest match of RegExp in String.

match(S, RegExp) when is_list(RegExp) ->
    case parse(RegExp) of
	{ok,RE} -> match(S, RE);
	{error,E} -> {error,E}
    end;
match(S, {nfa,NFA}) when is_binary(S) ->
    case match_bin(S, 1, NFA, 0, -1) of
	{Start,Len} when Len >= 0 -> {match,Start,Len};
	{_,_} -> nomatch
    end;
match(S, {nfa,NFA}) ->
    case match_str(S, 1, NFA, 0, -1) of
	{Start,Len} when Len >= 0 -> {match,Start,Len};
	{_,_} -> nomatch
    end.

match_str(Cs0, P, Nfa, Mst, Mlen) ->
    case next_match_str(Cs0, P, Nfa) of
	{match,St,Len,[_|Cs],_} ->
	    if Len > Mlen -> match_str(Cs, St+1, Nfa, St, Len);
	       true -> match_str(Cs, St+1, Nfa, Mst, Mlen)
	    end;
	{match,St,Len,[],_} ->			%Empty match at end
	    if Len > Mlen -> {St,Len};
	       true -> {Mst,Mlen}
	    end;
	nomatch -> {Mst,Mlen}
    end.

match_bin(Bin, P, Nfa, Mst, Mlen) ->
    case next_match_bin(Bin, P, Nfa) of
	{match,St,Len} when St+Len == size(Bin) -> %Empty match at end
	    if Len > Mlen -> {St,Len};
	       true -> {Mst,Mlen}
	    end;
	{match,St,Len} ->
	    if Len > Mlen -> match_bin(Bin, St+1, Nfa, St, Len);
	       true -> match_bin(Bin, St+1, Nfa, Mst, Mlen)
	    end;
	nomatch -> {Mst,Mlen}
    end.

%% match1(String, RegExp) -> {match,Start,Length} | nomatch | {error,E}.
%% first_match(String, RegExp) -> {match,Start,Length} | nomatch | {error,E}.
%%  Find the first match of RegExp in String, return Start and Length.

first_match(S, RegExp) when is_list(RegExp) ->
    {ok,RE} = parse(RegExp),
    first_match(S, RE);
first_match(S, {nfa,RE}) when is_binary(S) ->
    first_match_bin(S, 1, RE);
first_match(S, {nfa,RE}) ->
    first_match_str(S, 1, RE).

first_match_str(Cs, P, Nfa) ->
    case next_match_str(Cs, P, Nfa) of
	{match,St,Len,_,_} -> {match,St,Len};
	nomatch -> nomatch
    end.

first_match_bin(Bin, P0, Nfa) ->
    case next_match_bin(Bin, P0, Nfa) of
	{match,St,Len} -> {match,St,Len};
	nomatch -> nomatch
    end.

%% smatch(String, RegExp) ->
%%      {match,Start,Length,String,SubExprs} | nomatch | {error,E}.
%%  Find the longest match of RegExp in String.

smatch(S, RegExp) when is_list(RegExp) ->
    case parse(RegExp) of
	{ok,RE} -> smatch(S, RE);
	{error,E} -> {error,E}
    end;
smatch(S, {nfa,Nfa}) when is_binary(S) ->
    case smatch_bin(S, 1, Nfa, {0,-1,none}) of
	{St,Len,Subs} when Len >= 0 ->
	    {match,St,Len,bin_to_list(S, St, Len),fix_subs_bin(Subs, S)};
	{_,_,_} -> nomatch
    end;
smatch(S, {nfa,Nfa}) ->
    case smatch_str(S, 1, Nfa, {0,-1,[],none}) of
	{St,Len,Cs,Subs} when Len >= 0 ->
	    {match,St,Len,substr(Cs, 1, Len),fix_subs_str(Subs, St, Cs)};
	{_,_,_,_} -> nomatch
    end.

smatch_str(Cs0, P, Nfa, {_,Mlen,_,_}=M) ->
    case next_smatch_str(Cs0, P, Nfa) of
	{match,St,Len,[_|Cs]=Cs1,Subs,_} ->		%Found a match
	    if Len > Mlen -> smatch_str(Cs, St+1, Nfa, {St,Len,Cs1,Subs});
	       true -> smatch_str(Cs, St+1, Nfa, M)
	    end;
	{match,St,Len,[],Subs,_} ->
	    if Len > Mlen -> {St,Len,[],Subs};
	       true -> M
	    end;
	nomatch -> M
    end.

smatch_bin(Bin, P, Nfa, {_,Mlen,_}=M) ->
    case next_smatch_bin(Bin, P, Nfa) of
	{match,St,Len,Subs} when St+Len == size(Bin) ->
	    if Len > Mlen -> {St,Len,Subs};
	       true -> M
	    end;
	{match,St,Len,Subs} ->
	    if Len > Mlen -> smatch_bin(Bin, St+1, Nfa, {St,Len,Subs});
	       true -> smatch_bin(Bin, St+1, Nfa, M)
	    end;
	nomatch -> M
    end.

%% first_smatch(String, RegExp) ->
%%       {match,Start,Length,SubExprs} | nomatch | {error,E}.
%%  Find the longest match of RegExp in String, return Start and Length
%%  as well as tuple of sub-expression matches.

first_smatch(S, RegExp) when is_list(RegExp) ->
    {ok,RE} = parse(RegExp),
    first_smatch(S, RE);
first_smatch(S, {nfa,RE}) when is_binary(S) ->
    first_smatch_bin(S, 1, RE);
first_smatch(S, {nfa,RE}) ->
    first_smatch_str(S, 1, RE).

first_smatch_str(Cs, P, Nfa) ->
    case next_smatch_str(Cs, P, Nfa) of
	{match,St,Len,_,Subs,_} -> {match,St,Len,fix_subs_str(Subs,1,Cs)};
	nomatch -> nomatch
    end.

first_smatch_bin(Bin, P, Nfa) ->
    case next_smatch_bin(Bin, P, Nfa) of
	{match,St,Len,Subs} -> {match,St,Len,fix_subs_bin(Subs, Bin)};
	nomatch -> nomatch
    end.

%% matches(String, RegExp) -> {match,[{Start,Length}]} | {error,E}.
%%  Return the all the non-overlapping matches of RegExp in String.

matches(S, RegExp) when is_list(RegExp) ->
    case parse(RegExp) of
	{ok,RE} -> matches(S, RE);
	{error,E} -> {error,E}
    end;
matches(S, {nfa,NFA}) when is_binary(S) ->
    {match,matches_bin(S, 1, NFA)};
matches(S, {nfa,NFA}) ->
    {match,matches_str(S, 1, NFA)}.

matches_str(Cs0, P0, Nfa) ->
    case next_match_str(Cs0, P0, Nfa) of
	{match,St,0,_,[_|Cs1]} ->
	    [{St,0}|matches_str(Cs1, St+1, Nfa)];
	{match,St,0,_,[]} -> [{St,0}];
	{match,St,Len,_,Cs1} ->
	    [{St,Len}|matches_str(Cs1, St+Len, Nfa)];
	nomatch -> []
    end.

matches_bin(Bin, P0, Nfa) ->
    case next_match_bin(Bin, P0, Nfa) of
	{match,St,0} when St =< size(Bin) ->
	    [{St,0}|matches_bin(Bin, St+1, Nfa)];
	{match,St,0} -> [{St,0}];
	{match,St,Len} ->
	    [{St,Len}|matches_bin(Bin, St+Len, Nfa)];
	nomatch -> []
    end.

%% sub(String, RegExp, Replace) -> {ok,RepString,RepCount} | {error,E}.
%%  Substitute the first match of the regular expression RegExp with
%%  the string Replace in String. Accept pre-parsed regular
%%  expressions.

sub(S, RegExp, Rep) when is_list(RegExp) ->
    case parse(RegExp) of
	{ok,RE} -> sub(S, RE, Rep);
	{error,E} -> {error,E}
    end;
sub(S, {nfa,Nfa}, Rep) when is_binary(S) ->
    case sub_bin(S, 1, Nfa, Rep) of
	{yes,NewBin} -> {ok,list_to_binary(NewBin),1};
	no -> {ok,S,0}
    end;
sub(S, {nfa,Nfa}, Rep) ->
    case sub_str(S, 1, Nfa, Rep) of
	{yes,NewStr} -> {ok,NewStr,1};
	no -> {ok,S,0}
    end.

%% sub_str(String, Position, NFA, Replacement) ->
%%      {yes,NewString} | no.
%% sub_bin(String, Position, NFA, Replacement) ->
%%      {yes,NewString} | no.
%% Step forward over String until a match is found saving stepped over
%% chars in Before. Return reversed Before prepended to replacement
%% and rest of string.

sub_str(Cs0, P, Nfa, Rep) ->
    case next_match_str(Cs0, P, Nfa) of
	{match,St,Len,Cs,Cs1} ->
	    {yes,substr_app(St-P, Cs0,
			    sub_repl(Rep, substr(Cs, 1, Len), Cs1))};
	nomatch -> no
    end.

substr_app(0, _, App) -> App;
substr_app(N, [C|Cs], App) ->
    [C|substr_app(N-1, Cs, App)];
substr_app(_, [], App) -> App.

sub_bin(Bin, P, Nfa, Rep) ->
    case next_match_bin(Bin, P, Nfa) of
	{match,St,Len} ->
	    {yes,[sub_bin(Bin, P, St - P),
		  sub_repl(Rep, binary_to_list(Bin, St, St+Len-1),
			   sub_bin(Bin, St+Len))]};
	nomatch -> no
    end.

sub_repl([$&|Rep], M, Rest) -> M ++ sub_repl(Rep, M, Rest);
sub_repl([$\\,$&|Rep], M, Rest) -> [$&|sub_repl(Rep, M, Rest)];
sub_repl([C|Rep], M, Rest) -> [C|sub_repl(Rep, M, Rest)];
sub_repl([], _M, Rest) -> Rest.

%%  gsub(String, RegExp, Replace) -> {ok,RepString,RepCount} | {error,E}.
%%  Substitute every match of the regular expression RegExp with the
%%  string New in String. Accept pre-parsed regular expressions.

gsub(S, RegExp, Rep) when is_list(RegExp) ->
    case parse(RegExp) of
	{ok,RE} -> gsub(S, RE, Rep);
	{error,E} -> {error,E}
    end;
gsub(S, {nfa,Nfa}, Rep) when is_binary(S) ->
    case gsub_bin(S, 1, Nfa, Rep) of
	{NewStr,N} -> {ok,list_to_binary(NewStr),N};
	no -> {ok,S,0}				%No substitutions
    end;
gsub(S, {nfa,Nfa}, Rep) ->
    case gsub_str(S, 1, Nfa, Rep) of
	{NewStr,N} -> {ok,NewStr,N};
	no -> {ok,S,0}				%No substitutions
    end.

%% gsub_str(String, Position, NFA, Replacement) ->
%%      {NewString,Count} | no.
%% Step forward over String until a match is found saving stepped over
%% chars in Before. Call recursively to do rest of string after
%% match. Return reversed Before prepended to return from recursive
%% call.

gsub_str(Cs0, P, Nfa, Rep) ->
    case next_match_str(Cs0, P, Nfa) of
	{match,St,0,_,[C|Cs1]} ->
	    {New,N} = gsub_str(Cs1, St+1, Nfa, Rep),
	    {substr_app(St-P, Cs0, sub_repl(Rep, [], [C|New])),N+1};
	{match,_,0,_,[]} -> {sub_repl(Rep, [], []),1};
	{match,St,Len,Cs,Cs1} ->
	    {New,N} = gsub_str(Cs1, St+Len, Nfa, Rep),
	    {substr_app(St-P, Cs0,
			sub_repl(Rep, substr(Cs, 1, Len), New)),N+1};
	nomatch -> {Cs0,0}
    end.

gsub_bin(Bin, P, Nfa, Rep) ->
    case next_match_bin(Bin, P, Nfa) of
	{match,St,0} when St =< size(Bin) ->
	    {New,N} = gsub_bin(Bin, St+1, Nfa, Rep),
	    New1 = binary_to_list(Bin, St, St) ++ New,
	    {[sub_bin(Bin, P, St - P), sub_repl(Rep, [], New1)],N+1};
	{match,_,0} -> {sub_repl(Rep, [], []),1};
	{match,St,Len} ->
	    {New,N} = gsub_bin(Bin, St+Len, Nfa, Rep),
	    {[sub_bin(Bin, P, St - P),
	      sub_repl(Rep, binary_to_list(Bin, St, St+Len-1), New)], N+1};
	nomatch -> {sub_bin(Bin, P),0}
    end.

%% split(String, RegExp) -> {ok,[SubString]} | {error,E}.
%%  Split a string into substrings where the RegExp describes the
%%  field seperator. The RegExp " " is specially treated.

split(S, " ") -> split(S, "[ \t]+", true);	%This is really special!
split(S, Regexp) -> split(S, Regexp, false).

split(S, Regexp, Trim) when is_list(Regexp) ->
    case parse(Regexp) of
	{ok,RE} -> split(S, RE, Trim);
	{error,E} -> {error,E}
    end;
split(S, {nfa,Nfa}, Trim) when is_binary(S) ->
    case split_bin(S, 1, Nfa, Trim) of
	[[]|Ss] when Trim -> {ok,Ss};
	Ss -> {ok,Ss}
    end;
split(S, {nfa,Nfa}, Trim) ->
    case split_str(S, 1, Nfa, Trim) of
	[[]|Ss] when Trim -> {ok,Ss};
	Ss -> {ok,Ss}
    end.

split_str(Cs0, P, Nfa, Trim) ->
    case next_match_str(Cs0, P, Nfa) of
	{match,St,0,_,[C|Cs1]} ->
	    Ss1 = case split_str(Cs1, St+1, Nfa, Trim) of
		      [S1|Ss] -> [[C|S1]|Ss];
		      [] -> [[C]]
		  end,
	    [substr(Cs0, 1, St-P)|Ss1];
	{match,St,0,_,[]} -> [substr(Cs0, 1, St-P)];
	{match,St,Len,_,Cs1} ->
	    [substr(Cs0, 1, St-P)|split_str(Cs1, St+Len, Nfa, Trim)];
	nomatch ->
	    if Trim, Cs0 == [] -> [];
	       true -> [Cs0]
	    end
    end.

split_bin(Bin, P, Nfa, Trim) ->
    case next_match_bin(Bin, P, Nfa) of
	{match,St,0} when St =< size(Bin) ->
	    C = bin_to_list(Bin, St, 1),
	    Ss1 = case split_bin(Bin, St+1, Nfa, Trim) of
		      [S1|Ss] -> [list_to_binary([C|S1])|Ss];
		      [] -> [C]
		  end,
	    [sub_bin(Bin, P, St-P)|Ss1];
	{match,St,0} -> [sub_bin(Bin, P, St-P)];
	{match,St,Len} ->
	    [sub_bin(Bin, P, St-P)|split_bin(Bin, St+Len, Nfa, Trim)];
	nomatch ->
	    if Trim, P > size(Bin) -> [];
	       P > size(Bin) -> [<<>>];
	       true -> [sub_bin(Bin, P)]
	    end
    end.

fix_subs_str(Subs, St, S) ->
    Subsl = fix_subs_str(Subs, St, S, size(Subs), []),
    list_to_tuple(Subsl).

fix_subs_str(_, _, _, 0, Ss) -> Ss;
fix_subs_str(Subs, P, S, N, Ss) ->
     E = case element(N, Subs) of
	     {St,L} -> {-St,L,substr(S, -St-P+1, L)};
	     undefined -> undefined
	 end,
    fix_subs_str(Subs, P, S, N-1, [E|Ss]).

fix_subs_bin(Subs, Bin) ->
    Subsl = fix_subs_bin(Subs, Bin, size(Subs), []),
    list_to_tuple(Subsl).

fix_subs_bin(_, _, 0, Ss) -> Ss;
fix_subs_bin(Subs, Bin, N, Ss) ->
     E = case element(N, Subs) of
	     {St,L} -> {-St,L,bin_to_list(Bin, -St, L)};
	     undefined -> undefined
	 end,
    fix_subs_bin(Subs, Bin, N-1, [E|Ss]).

%% bin_to_list(Binary, Start) -> Chars.
%% bin_to_list(Binary, Start, Length) -> Chars.
%%  As it should be!

% bin_to_list(Bin, St) -> binary_to_list(Bin, St, size(Bin)).

bin_to_list(_, _, 0) -> [];
bin_to_list(Bin, St, L) -> binary_to_list(Bin, St, St+L-1).

sub_bin(Bin, St) ->
    St1 = St - 1,
    <<_:St1/binary,Sub/binary>> = Bin,
    Sub.

sub_bin(Bin, St, Len) ->
    St1 = St - 1,
    <<_:St1/binary,Sub:Len/binary,_/binary>> = Bin,
    Sub.

%% The NFA engines.
%%  We have two separate engines depending on whether we want to
%%  capture sub-expressions. Both have a top-level driver for strings
%%  and binaries. We need to do one character lookahead to get correct
%%  end of string behaviour as we match both [] and [$\n]. This is a
%%  pain!

%% next_match_str(String, StartPos, NFA) ->
%%      {match,Start,Length,Chars,RestChars} | nomatch.
%%  Find the next match in String. Try successive positions until
%%  either a match is found or we reach the end of the string.

next_match_str(Cs, P, {Nfa,Start,_}) ->
    next_match_str(Cs, P, Nfa, eclosure(Start, Nfa, [], [])).

next_match_str([_|Cs1]=Cs0, P0, Nfa, Ss) ->
    case nfa_str(Cs0, P0, Nfa, Ss, nomatch) of
	{match,P1,Cs} -> {match,P0,P1-P0,Cs0,Cs};
	nomatch -> next_match_str(Cs1, P0+1, Nfa, Ss)
    end;
next_match_str([], P0, Nfa, Ss) ->
    case nfa_str([], P0, Nfa, Ss, nomatch) of	%Try for null match at end.
	{match,P1,Cs} -> {match,P0,P1-P0,[],Cs};
	nomatch -> nomatch
    end.

%% nfa_str(Chars, Pos, NFA, States, Accept) -> {match,NextPos,Rest} | nomatch.
%%  Run the NFA machine over binary starting at one position until we
%%  either have a match or not a match.

nfa_str(_, _, _, [], A) -> A;			%No matching states
nfa_str([C|[C1|_]=Cs1]=Cs0, P, Nfa, Ss0, A) ->
    Gl = {P,C,C1},
    case step(C, Gl, Nfa, Ss0, [], false) of
	{Ss1,true} ->
	    nfa_str(Cs1, P+1, Nfa, Ss1, {match,P,Cs0});
	{Ss1,false} ->
	    nfa_str(Cs1, P+1, Nfa, Ss1, A)
    end;
nfa_str([C]=Cs0, P, Nfa, Ss0, A) ->
    Gl = {P,C,eos},
    case step(C, Gl, Nfa, Ss0, [], false) of
	{Ss1,true} ->
	    nfa_str([], P+1, Nfa, Ss1, {match,P,Cs0});
	{Ss1,false} ->
	    nfa_str([], P+1, Nfa, Ss1, A)
    end;
nfa_str([], P, Nfa, Ss, A) ->			%No more characters
    case has_match(P, Nfa, Ss) of
	yes -> {match,P,[]};
	no -> A					%Take what we got
    end.

%% next_match_bin(Binary, StartPos, NFA) ->
%%      {match,Start,Length} | nomatch.
%%  Find the next match in Binary. Try successive positions until
%%  either a match is found or we reach the end of the string.

next_match_bin(Bin, P, {Nfa,Start,_}) ->
    next_match_bin(Bin, P, Nfa, eclosure(Start, Nfa, [], [])).

next_match_bin(Bin, P0, Nfa, Ss) when P0 < size(Bin) ->
    case nfa_bin(Bin, P0, Nfa, Ss, nomatch) of
	{match,P1} -> {match,P0,P1-P0};
	nomatch -> next_match_bin(Bin, P0+1, Nfa, Ss)
    end;
next_match_bin(Bin, P0, Nfa, Ss) ->
    case nfa_bin(Bin, P0, Nfa, Ss, nomatch) of	%Try for null match at end.
	{match,P1} -> {match,P0,P1-P0};
	nomatch -> nomatch
    end.

%% nfa_bin(Binary, Pos, NFA, States, Accept) -> {match,NextPos} | nomatch.
%%  Run the NFA machine over binary starting at one position until we
%%  either have a match or not a match.

nfa_bin(_, _, _, [], A) -> A;			%No matching states
nfa_bin(Bin, P, Nfa, Ss0, A) ->
    P1 = P-1,					%Number of chars before
    case Bin of
	<<_:P1/binary,C,C1,_/binary>> ->
	    Gl = {P,C,C1},
	    case step(C, Gl, Nfa, Ss0, [], false) of
		{Ss1,true} ->
		    nfa_bin(Bin, P+1, Nfa, Ss1, {match,P});
		{Ss1,false} ->
		    nfa_bin(Bin, P+1, Nfa, Ss1, A)
	    end;
	<<_:P1/binary,C,_/binary>> ->
	    Gl = {P,C,eos},
	    case step(C, Gl, Nfa, Ss0, [], false) of
		{Ss1,true} ->
		    nfa_bin(Bin, P+1, Nfa, Ss1, {match,P});
		{Ss1,false} ->
		    nfa_bin(Bin, P+1, Nfa, Ss1, A)
	    end;
	_ ->					%No more characters.
	    case has_match(P, Nfa, Ss0) of
		yes -> {match,P};
		no -> A				%Take what we got
	    end
    end.

%% step(Char, GlobalState, NFA, States, NewStates, Done) -> {NewStates,Done}.
%%  Pos is the position of the current character.

step(C, Gl, Nfa, [S|Ss], News, D) ->
    case element(S, Nfa) of
	#cstate{c=C,s=N} ->
	    step(C, Gl, Nfa, Ss, eclosure(N, Nfa, [], News), D);
	#cstate{c=done} -> step(C, Gl, Nfa, Ss, News, true);
	#cstate{} -> step(C, Gl, Nfa, Ss, News, D);
	#nstate{cc=Cc,s=N} ->
	    case match_char(C, Cc) of
		true ->
		    step(C, Gl, Nfa, Ss, eclosure(N, Nfa, [], News), D);
		false -> step(C, Gl, Nfa, Ss, News, D)
	    end;
	#pstate{t=bos,s=N} ->
	    if element(1, Gl) == 1 ->
		    %% Add eclosure to *this* level of states
		    Ss1 = eclosure(N, Nfa, [], Ss),
		    step(C, Gl, Nfa, Ss1, News, D);
	       true -> step(C, Gl, Nfa, Ss, News, D)
	    end;
	#pstate{t=eos,s=N} ->
	    Ss1 = if element(2, Gl) == $\n, element(3, Gl) == eos ->
			  %% Add eclosure to *this* level of states
			  eclosure(N, Nfa, [], Ss);
		     true -> Ss
		  end,
	    step(C, Gl, Nfa, Ss1, News, D)
    end;
step(_, _, _, [], News, D) -> {News,D}.

%% eclosure(State, Nfa, SeenStates, NewStates) -> NewStates.

eclosure(S, Nfa, Es, Rest) ->
    case element(S, Nfa) of
	#estate{s1=S1,s2=S2} ->
	    %% Must track of where we have been to avoid loops.
	    case member(S, Es) of
		true -> Rest;
		false ->
		    Es1 = [S|Es],
		    eclosure(S1, Nfa, Es1, eclosure(S2, Nfa, Es1, Rest))
	    end;
	%% Just ignore parentheses states here.
	#lstate{s=S1} -> eclosure(S1, Nfa, Es, Rest);
	#rstate{s=S1} -> eclosure(S1, Nfa, Es, Rest);
	%% All other states get added to state list.
	_St -> add_state(S, Rest, Rest)
    end.

%% add_state(State, States, States) -> States.
%% Add a state to list of states. As list generally short it is better
%% to carry it around in extra argument and prepend new to beginning
%% rather than rebuilding every call.

add_state(S, [S|_Ss], All) -> All;
add_state(S, [_|Ss], All) -> add_state(S, Ss, All);
add_state(S, [], All) -> [S|All].

%% match_char(Char, Class) -> bool().

match_char(C, [{C1,C2}|_Cc]) when C >= C1, C =< C2 -> true;
match_char(C, [C|_Cc]) -> true;
match_char(C, [_|Cc]) -> match_char(C, Cc);
match_char(_, []) -> false.

has_match(P, Nfa, [S|Ss]) ->
    case element(S, Nfa) of
	#cstate{c=done} -> yes;
	#pstate{t=bos,s=N} ->
	    if P == 1 ->
		    %% Add eclosure to *this* level of states
		    Ss1 = eclosure(N, Nfa, [], Ss),
		    has_match(P, Nfa, Ss1);
	       true -> has_match(P, Nfa, Ss)
	    end;
	#pstate{t=eos,s=N} ->			%EOS is always valid here
	    Ss1 = eclosure(N, Nfa, [], Ss),
	    has_match(P, Nfa, Ss1);
	_ -> has_match(P, Nfa, Ss)
    end;
has_match(_, _, []) -> no.

%% next_smatch_str(String, StartPos, NFA) ->
%%      {match,Start,Length,Chars,Subs,RestChars} | nomatch.
%%  Find the next match in String. Try successive positions until
%%  either a match is found or we reach the end of the string.

next_smatch_str(Cs, P, {Nfa,Start,Sc}) ->
    Subs = erlang:make_tuple(Sc, undefined),
    next_smatch_str(Cs, P, Nfa, Start, Subs).

next_smatch_str([_|Cs1]=Cs0, P0, Nfa, Start, Subs0) ->
    Ss = eclosure_s(Start, Nfa, [], [], P0, Subs0),
    case nfa_str_s(Cs0, P0, Nfa, Ss, nomatch) of
	{match,Subs1,P1,Cs} -> {match,P0,P1-P0,Cs0,Subs1,Cs};
	nomatch -> next_smatch_str(Cs1, P0+1, Nfa, Start, Subs0)
    end;
next_smatch_str([], P0, Nfa, Start, Subs0) ->
    Ss = eclosure_s(Start, Nfa, [], [], P0, Subs0),
    case nfa_str_s([], P0, Nfa, Ss, nomatch) of	%Try for null match at end.
	{match,Subs1,P1,Cs} -> {match,P0,P1-P0,[],Subs1,Cs};
	nomatch -> nomatch
    end.

%% Must do eclosure and parentheses marking when we have a new
%% character.

%% nfa_str_s(Chars, Pos, NFA, States, Accept) ->
%%      {match,Subs,NextPos,RestCars} | nomatch.
%%  Run the NFA machine over binary starting at one position until we
%%  either have a match or not a match.

nfa_str_s(_, _, _, [], A) -> A;		%No matching states
nfa_str_s([C|[C1|_]=Cs1]=Cs0, P, Nfa, Ss0, A0) ->
    Gl = {P,C,C1},
    ?TP("N: ~w x ~w x ~w\n", [Gl,Ss0,A0]),
    case step_s(C, Gl, Nfa, Ss0, [], none) of
	{Ss1,none} ->
	    ?TP("N: => ~w x ~w\n", [Ss1,none]),
	    nfa_str_s(Cs1, P+1, Nfa, Ss1, A0);
	{Ss1,Subs} ->
	    ?TP("N: => ~w x ~w\n", [Ss1,Subs]),
	    nfa_str_s(Cs1, P+1, Nfa, Ss1, {match,Subs,P,Cs0})
    end;
nfa_str_s([C]=Cs0, P, Nfa, Ss0, A0) ->
    Gl = {P,C,eos},
    ?TP("N: ~w x ~w x ~w\n", [Gl,Ss0,A0]),
    case step_s(C, Gl, Nfa, Ss0, [], none) of
	{Ss1,none} ->
	    ?TP("N: => ~w x ~w\n", [Ss1,none]),
	    nfa_str_s([], P+1, Nfa, Ss1, A0);
	{Ss1,Subs} ->
	    ?TP("N: => ~w x ~w\n", [Ss1,Subs]),
	    nfa_str_s([], P+1, Nfa, Ss1, {match,Subs,P,Cs0})
    end;
nfa_str_s([], P, Nfa, Ss, A) ->			%No more characters
    case has_match_s(P, Nfa, Ss) of
	{yes,Subs} -> {match,Subs,P,[]};
	no -> A					%Take what we got
    end.

%% next_smatch_bin(Binary, StartPos, NFA) ->
%%      {match,Start,Length,Subs} | nomatch.
%%  Find the next match in Binary. Try successive positions until
%%  either a match is found or we reach the end of the string.

next_smatch_bin(Bin, P, {Nfa,Start,Sc}) ->
    Subs = erlang:make_tuple(Sc, undefined),
    next_smatch_bin(Bin, P, Nfa, Start, Subs).

next_smatch_bin(Bin, P0, Nfa, Start, Subs0) when P0 < size(Bin) ->
    Ss = eclosure_s(Start, Nfa, [], [], P0, Subs0),
    case nfa_bin_s(Bin, P0, Nfa, Ss, nomatch) of
	{match,Subs1,P1} -> {match,P0,P1-P0,Subs1};
	nomatch -> next_smatch_bin(Bin, P0+1, Nfa, Start, Subs0)
    end;
next_smatch_bin(Bin, P0, Nfa, Start, Subs0) ->
    Ss = eclosure_s(Start, Nfa, [], [], P0, Subs0),
    %% Try for null match at end.
    case nfa_bin_s(Bin, P0, Nfa, Ss, nomatch) of
	{match,Subs1,P1} -> {match,P0,P1-P0,Subs1};
	nomatch -> nomatch
    end.

%% nfa_bin_s(Binary, Pos, NFA, States, Accept) ->
%%      {match,Subs,NextPos} | nomatch.
%%  Run the NFA machine over binary starting at one position until we
%%  either have a match or not a match.

nfa_bin_s(_, _, _, [], A) -> A;			%No matching states
nfa_bin_s(Bin, P, Nfa, Ss0, A) ->
    P1 = P-1,					%Number of chars before
    case Bin of
	<<_:P1/binary,C,C1,_/binary>> ->
	    Gl = {P,C,C1},
	    case step_s(C, Gl, Nfa, Ss0, [], none) of
		{Ss1,none} ->
		    nfa_bin_s(Bin, P+1, Nfa, Ss1, A);
		{Ss1,Subs} ->
		    nfa_bin_s(Bin, P+1, Nfa, Ss1, {match,Subs,P})
	    end;
	<<_:P1/binary,C,_/binary>> ->
	    Gl = {P,C,eos},
	    case step_s(C, Gl, Nfa, Ss0, [], none) of
		{Ss1,none} ->
		    nfa_bin_s(Bin, P+1, Nfa, Ss1, A);
		{Ss1,Subs} ->
		    nfa_bin_s(Bin, P+1, Nfa, Ss1, {match,Subs,P})
	    end;
	_ ->					%No more characters.
	    case has_match_s(P, Nfa, Ss0) of
		{yes,Subs} -> {match,Subs,P};
		no -> A				%Take what we got
	    end
    end.

%% step_s(Char, GlobalState, NFA, States, NewStates, BestThread) ->
%%      {NewStates,NewBest}.
%%  Pos is the position of the current character.

step_s(C, {P,_,_}=Gl, Nfa, [{S,Subs}|Ss], News0, Best) ->
    case element(S, Nfa) of
	#cstate{c=C,s=N} ->
	    News1 = eclosure_s(N, Nfa, [], News0, P+1, Subs),
	    step_s(C, Gl, Nfa, Ss, News1, Best);
	#cstate{c=done} ->
	    step_s(C, Gl, Nfa, Ss, News0, best_subs(Best, Subs));
	#cstate{} -> step_s(C, Gl, Nfa, Ss, News0, Best);
	#nstate{cc=Cc,s=N} ->
	    case match_char(C, Cc) of
		true ->
		    News1 = eclosure_s(N, Nfa, [], News0, P+1, Subs),
		    step_s(C, Gl, Nfa, Ss, News1, Best);
		false -> step_s(C, Gl, Nfa, Ss, News0, Best)
	    end;
	#pstate{t=bos,s=N} ->
	    if P == 1 ->
		    %% Add eclosure to *this* level of states
		    Ss1 = eclosure_s(N, Nfa, [], Ss, P, Subs),
		    step_s(C, Gl, Nfa, Ss1, News0, Best);
	       true -> step_s(C, Gl, Nfa, Ss, News0, Best)
	    end;
	#pstate{t=eos,s=N} ->
	    Ss1 = if element(2, Gl) == $\n, element(3, Gl) == eos ->
			  %% Add eclosure to *this* level of states
			  eclosure_s(N, Nfa, [], Ss, P, Subs);
		     true -> Ss
		  end,
	    step_s(C, Gl, Nfa, Ss1, News0, Best)
    end;
step_s(_, _, _, [], News, Best) -> {News,Best}.

%% eclosure_s(State, Nfa, SeenEstates, NewStates, Pos, Subs) -> NewStates.
%%  Pos is the position of the *next* character to be processed.

eclosure_s(S, Nfa, Es, Ss0, P, Subs0) ->
    case element(S, Nfa) of
	#estate{s1=S1,s2=S2} ->
	    case member(S, Es) of
		true -> Ss0;
		false ->
		    Es1 = [S|Es],
		    Ss1 = eclosure_s(S1, Nfa, Es1, Ss0, P, Subs0),
		    eclosure_s(S2, Nfa, Es1, Ss1, P, Subs0)
	    end;
	#lstate{s=S1,n=N} ->
	    Subs1 = add_lparen(N, P, Subs0),
	    eclosure_s(S1, Nfa, Es, Ss0, P, Subs1);
	#rstate{s=S1,n=N} ->
	    Subs1 = add_rparen(N, P, Subs0),
	    eclosure_s(S1, Nfa, Es, Ss0, P, Subs1);
	%% All other states get added to state list.
	_ -> add_state_s(S, Subs0, Ss0, Ss0)
    end.

%% Want the longest leftmost for sub exprs by saving each parenthesis
%% pair as {-Start,Length} | undefined then a simple comparison of the
%% subs tuples gives the right answer. Bigger is better!

%% best_subs(OldSubs, NewSubs) -> BestSubs.

best_subs(Old, New) when Old >= New -> Old;
best_subs(_Old, New) -> New.

add_state_s(S, Subs, [{S,Subs1}|_Ss], All) when Subs1 > Subs -> All;
add_state_s(S, Subs, [_|Ss], All) -> add_state_s(S, Subs, Ss, All);
add_state_s(S, Subs, [], All) -> [{S,Subs}|All].

add_lparen(N, P, Subs) ->
    ?TP("L: ~w\n", [{N,P,Subs}]),
    %%Negative start to make easier comparison, bigger is better!
    Pm = -P,
    case element(N, Subs) of
	undefined -> setelement(N, Subs, Pm);
	{P1,_} when Pm =< P1 -> Subs;
	P1 when Pm =< P1 -> Subs
    end.

add_rparen(N, P, Subs) ->
    ?TP("R: ~w\n", [{N,P,Subs}]),
    case element(N, Subs) of
	P1 when is_integer(P1) ->
	    %% Negative start to make easier comparison, bigger is better!
	    setelement(N, Subs, {P1,P+P1});
	{_,P2} when P2 =< P -> Subs
    end.

has_match_s(P, Nfa, [{S,Subs}|Ss]) ->
    case element(S, Nfa) of
	#pstate{t=bos,s=N} ->
	    if P == 1 ->
		    %% Add eclosure to *this* level of states
		    Ss1 = eclosure_s(N, Nfa, [], Ss, P, Subs),
		    has_match_s(P, Nfa, Ss1);
	       true -> has_match_s(P, Nfa, Ss)
	    end;
	#pstate{t=eos,s=S1} ->			%EOS is always valid here
	    Ss1 = eclosure_s(S1, Nfa, [], Ss, P, Subs),
	    has_match_s(P, Nfa, Ss1);
	#cstate{c=done} ->
	    case has_match_s(P, Nfa, Ss) of
		{yes,Subs1}=Yes when Subs1 > Subs -> Yes;
		_ -> {yes,Subs}
	    end;
	_ -> has_match_s(P, Nfa, Ss)
    end;
has_match_s(_, _, []) -> no.

tt(N, F) ->
    statistics(runtime),
    statistics(reductions),
    Res = tt_loop(N, F),
    {_,Reds} = statistics(reductions),
    {_,Cpu} = statistics(runtime),
    {Res,Reds,Cpu}.

tt_loop(N, F) when N > 1 ->
    F(), tt_loop(N-1, F);
tt_loop(1, F) -> F();
tt_loop(0, _) -> none.

loadf(File) ->
    {ok,B} = file:read_file(File),
    binary_to_list(B).
