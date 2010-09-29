%%%----------------------------------------------------------------------
%%% File    : apache_property_mlex.erl
%%% Author  :  <svg@surnet.ru>
%%% Purpose : Free Radius test grammar
%%% Created : 29 Jul 2002 by  <svg@surnet.ru>
%%%----------------------------------------------------------------------
%%%
%%% $Id: apache_property_mlex.erl,v 1.1.1.1 2002/10/04 00:00:00 svg Exp $
%%%
%%% $Log: apache_property_mlex.erl,v $
%%% Revision 1.1.1.1  2002/10/04 00:00:00  svg
%%% Imported sources
%%%
%%% Revision 1.1.1.1  2002/09/04 20:19:07  svg
%%% Imported sources
%%%
%%%

-module(apache_property_mlex).
-author('svg@surnet.ru').

-import(mlex,[c/1, nc/1, ci/1, ci/2, cni/1, cni/2, str/1, btw/3]).
-import(mlex,['@'/1, '|'/1, '*'/1, '+'/1, '?'/1, '.'/0, sol/1, eol/1]).

-export([apache_grammar/0, clike_grammar/0, create_env/0, drop_env/0]).

-define(ENV_PREFIX, atom_to_list(?MODULE)).
-define(skip, fun (_, _, _) -> [] end).

apache_grammar() ->
  Grammar = 
    mlex:grammar(
      [{decl_end,       decl_end(),       fun yeec_token/3},
       {ws,             ws(),             ?skip},
       {nl,             nl(),             ?skip},
       {shell_comment,  shell_comment(),  ?skip},
       {assign_op,      assign_op(),      fun yeec_token/3},
       {true_kw,        true_kw(),        fun yeec_token/3},
       {false_kw,       false_kw(),       fun yeec_token/3},
       {tag_open,       tag_open(),       fun yeec_token/3},
       {tag_close,      tag_close(),      fun yeec_token/3},
       {tag_end,        tag_end(),        fun yeec_token/3},
       {r_brace,        r_brace(),        fun yeec_token/3},
       {float_num,      float_num(),      fun yeec_token/3},
       {integer_num,    integer_num(),    fun yeec_token/3},
       {ipv4_address,   ipv4_address(),   fun yeec_token/3},
       {ipv4_netmask,   ipv4_netmask(),   fun yeec_token/3},
       {atom_string,    atom_string(),    fun yeec_token_apache/3},
       {var_begin,      var_begin(),      fun yeec_token/3},
       {unquote_substring1, unquote_substring1(), '/', var_begin(), fun yeec_token/3},
       {unquote_substring2, unquote_substring2(), fun yeec_token/3},
       {unquote_substring3, unquote_substring3(), '/', var_begin(), fun yeec_token/3},
       {dquote_substring1, dquote_substring1(), '/', var_begin(), fun yeec_token/3},
       {dquote_substring2, '@'([r_brace(), dquote_substring2()]), fun yeec_token/3},

       {unquote_string, unquote_string(), fun yeec_token/3},
       {dquote_string,  dquote_string(),  fun yeec_token/3},
       {squote_string,  squote_string(),  fun yeec_token/3}
      ]),
  {?MODULE, Grammar}.

clike_grammar() ->
  Grammar = 
    mlex:grammar(
      [{decl_end,       decl_end(),       fun yeec_token/3},
       {ws,             ws(),             ?skip},
       {nl,             nl(),             ?skip},
       {shell_comment,  shell_comment(),  ?skip},
       {true_kw,        true_kw(),        fun yeec_token/3},
       {false_kw,       false_kw(),       fun yeec_token/3},
       {assign_op,      assign_op(),      fun yeec_token/3},
       {operator,       operator(),       fun yeec_token/3},
       {r_brace,        r_brace(),        fun yeec_token/3},
       {l_brace,        l_brace(),        fun yeec_token/3},
       {float_num,      float_num(),      fun yeec_token/3},
       {integer_num,    integer_num(),    fun yeec_token/3},
       {ipv4_address,   ipv4_address(),   fun yeec_token/3},
       {ipv4_netmask,   ipv4_netmask(),   fun yeec_token/3},
       {atom_string,    atom_string(),    fun yeec_token/3},
       {var_begin,      var_begin(),      fun yeec_token/3},
       {unquote_substring1, unquote_substring1(), '/', var_begin(), fun yeec_token/3},
       {unquote_substring2, unquote_substring2(), fun yeec_token/3},
       {unquote_substring3, unquote_substring3(), '/', var_begin(), fun yeec_token/3},
       {dquote_substring1, dquote_substring1(), '/', var_begin(), fun yeec_token/3},
       {dquote_substring2, '@'([r_brace(), dquote_substring2()]), fun yeec_token/3},

       {unquote_string, unquote_string_clike(), fun yeec_token/3},
       {dquote_string,  dquote_string(),        fun yeec_token/3},
       {squote_string,  squote_string(),        fun yeec_token/3}
      ]),
  {?MODULE, Grammar}.

%% Keywords

true_kw() ->
  '|'([str("true"), str("ok"), str("yes")]).

false_kw() ->
  '|'([str("false"), str("no")]).

%%% Operators

%% Common
decl_end()    -> eol('*'(ws())).
r_brace()     -> c($}).
var_begin() -> str("${").
assign_op()   -> c($=).

%% Apache
tag_open() -> sol('@'(['*'(ws()), c($<)])).
tag_close() -> sol('@'(['*'(ws()), str("</")])).
tag_end() -> eol('@'([c($>), '*'(ws())])).

%% C-like
l_brace()     -> c(${).

%% Whitespace -- ignored

ws() ->	ci(" \t").

nl() -> '+'(ci("\r\n\f")).

shell_comment() -> '@'([c($#), '*'('.'())]).

operator() -> '@'([c($$), atom_string()]).

%% Strings
atom_string() -> '@'([alpha(), '*'('|'([alnum(), ci("-_")]))]).

unquote_substring1() -> unquote_string().

unquote_substring2() ->
  '@'([r_brace(), unquote_string()]).

unquote_substring3() ->
  '@'([r_brace(), '*'('|'([cni("\"$\r\n\f"), quote_chr()]))]).

unquote_string() ->
  '+'('|'([alnum(), quote_chr(), punct()])).

unquote_string_clike() ->
  '+'('|'([alnum(), quote_chr(), punct(), c($>)])).

dquote_substring1() ->
  '@'([c($"), '*'('|'([cni("\"$\r\n\f"), quote_chr()]))]).

dquote_substring2() ->
  '@'(['*'('|'([cni("\"$\r\n\f"), quote_chr()])), c($")]).

dquote_string() ->
  '@'([c($"), '*'('|'([cni([$", $$]), quote_chr()])), c($")]).

squote_string() ->
  '@'([c($'), '+'('|'([nc($'), quote_chr()])), c($')]).

quote_chr() -> '@'([c($\\), cni("\f\r\n")]).

%% IPv4 address

ipv4_address() ->
  '@'([btw(3, 3, '@'([btw(1, 3, digit()), c($.)])), btw(1, 3, digit())]).

ipv4_netmask() ->
  '@'([ipv4_address(), c($/), '|'([ipv4_address(), btw(1, 2, digit())])]).

%%Float
%%(+|-)?[0-9]+\.[0-9]+((E|e)(+|-)?[0-9]+)?
float_num() ->
  '@'([integer_num(), c($.),'+'(digit()),'?'('@'([ci("Ee"), integer_num()]))]).

integer_num() ->
  '@'(['?'(ci("+-")), '+'(digit())]).

alnum() ->
  '|'([alpha(), digit()]).

digit() ->
  ci($0, $9).

alpha() ->
  '|'([lalpha(), ualpha()]).

lalpha() ->
  '|'([ci($a, $z), ci(223, 255)]).

ualpha() ->
  '|'([ci($A, $Z), ci(192, 222)]).

%% skipped " ' ; \ # { } = $ >
punct() ->
  '|'([c($!), ci($%, $&), ci($(, $/), c($:), 
       c($<), ci($?, $@), c($[), ci($^, $`), c($|), c($~)]).

%%
%% Format funs
%%
yeec_token_apache(_Class=atom_string, Line, _Str="Include") ->
  {operator, Line, "INCLUDE"};
yeec_token_apache(Class, Line, Str) ->
  yeec_token(Class, Line, Str).

yeec_token(Class=integer_num, Line, Str) ->
  {Class, Line, list_to_integer(Str)};
yeec_token(Class=float_num, Line, Str) ->
  {Class, Line, list_to_float(Str)};
yeec_token(_Class=true_kw, Line, _) ->
  {bool, Line, true};
yeec_token(false_kw, Line, _) ->
  {bool, Line, false};
yeec_token(_Class=atom_string, Line, Str) ->
  {atom_string, Line, unquote_chars(Str)};
yeec_token(_Class=dquote_string, Line, Str) ->
  {string, Line, unquote_chars(string:strip(Str, both, $"))};
yeec_token(_Class=squote_string, Line, Str) ->
  {atom_string, Line, unquote_chars(string:strip(Str, both, $'))};
yeec_token(_Class=unquote_string, Line, Str) ->
  {string, Line, unquote_chars(Str)};
yeec_token(_Class=unquote_substring1, Line, Str) ->
  env_set(substr, unquote),
  {substring, Line, unquote_chars(Str)};

yeec_token(_Class=unquote_substring3, Line, Str) ->
  case env_get(substr) of
    StrType when StrType == unquote;
		 StrType == undefined ->
      Ss = split_string(tl(Str), " \t"),
      [{substring, Line, unquote_chars(hd(Ss))}|
       [{string, Line, unquote_chars(S)} || S <- tl(Ss)]];
    dquote ->
      {substring, Line, unquote_chars(tl(Str))}
  end;
yeec_token(_Class=unquote_substring2, Line, Str) ->
  {substring, Line, unquote_chars(tl(Str))};
yeec_token(_Class=dquote_substring1, Line, Str) ->
  env_set(substr, dquote),
  {substring, Line, unquote_chars(string:strip(Str, left, $"))};
yeec_token(_Class=dquote_substring2, Line, Str) ->
  {substring, Line, unquote_chars(tl(string:strip(Str, right, $")))};
yeec_token(Class, Line, Data) ->
  {Class, Line, Data}.

unquote_chars(Str) ->
  unquote_chars(Str, []).

unquote_chars([], Ret) ->
  lists:reverse(Ret);
unquote_chars([$\\, $^, C|Rest], Acc) when C >= $A,
					   C =< $Z ->
  unquote_chars(Rest, [C-65|Acc]);
unquote_chars([$\\, C3, C2, C1|Rest], Acc) when C3 =< 7,
						C2 =< 7,
						C1 =< 7 ->
  unquote_chars(Rest, [(C3*8 + C2)*8 +C1|Acc]);
unquote_chars([$\\, $n|Rest], Acc) ->
  unquote_chars(Rest, [$\n|Acc]);
unquote_chars([$\\, $r|Rest], Acc) ->
  unquote_chars(Rest, [$\r|Acc]);
unquote_chars([$\\, $a|Rest], Acc) ->
  unquote_chars(Rest, [$\a|Acc]);
unquote_chars([$\\, $b|Rest], Acc) ->
  unquote_chars(Rest, [$\b|Acc]);
unquote_chars([$\\, $t|Rest], Acc) ->
  unquote_chars(Rest, [$\t|Acc]);
unquote_chars([$\\, $v|Rest], Acc) ->
  unquote_chars(Rest, [$\v|Acc]);
unquote_chars([$\\, $f|Rest], Acc) ->
  unquote_chars(Rest, [$\f|Acc]);
unquote_chars([$\\, C|Rest], Acc) ->
  unquote_chars(Rest, [C|Acc]);
unquote_chars([C|Rest], Acc) ->
  unquote_chars(Rest, [C|Acc]).

%%
split_string(Str, Delims) ->
  split_string(Str, Delims, [[]]).

split_string([], _Delims, SubStrs) ->
  lists:reverse([lists:reverse(S) || S <- SubStrs]);
split_string([$\\, C|Str], Delims, [S|Ss]) ->
  split_string(Str, Delims, [[C, $\\| S]|Ss]);
split_string([C|Str], Delims, [S|Ss]) ->
  case lists:member(C, Delims) of
    true ->
      split_string(Str, Delims, [[],S|Ss]);
    false ->
      split_string(Str, Delims, [[C|S]|Ss])
  end.

create_env() ->
  env_name().

drop_env() ->
  env_name() ! stop.

env_set(Name, Val) ->
  env_name() ! {set, self(), {Name, Val}}.

env_get(Name) ->
  EP = env_name(),
  EP ! {get, self(), Name},
  receive
    {ok, EP, Val} ->
      Val
  end.

env_name() ->
  EN = list_to_atom(?ENV_PREFIX ++ pid_to_list(self())),
  case whereis(EN) of
    undefined ->
      SP = spawn_link(fun () -> env_create(self()) end),
      register(EN, SP),
      SP;
    Pid ->
      Pid
  end.

env_create(Parent) ->
  env_loop({Parent, dict:new()}).

env_loop({Parent, Dict}) ->
  receive
    {set, _Pid, {Name, Val}} ->
      Dict1 = dict:store(Name, Val, Dict),
      env_loop({Parent, Dict1});
    {get, Pid, Name} ->
      Ret =
	case dict:find(Name, Dict) of
	  error ->
	    undefined;
	  {ok, Val} ->
	    Val
	end,
      Pid ! {ok, self(), Ret},
      env_loop({Parent, Dict});
    stop ->
      exit(normal);
    {'EXIT', Parent, Reson} ->
      exit(Reson)
  end.
