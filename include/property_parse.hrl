%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id: property_parse.hrl,v 1.1.1.1 2002/10/04 00:00:00 svg Exp $
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%
-export([parse/2, parse/3, return_error/2, format_error/1]).

-define(THIS_MODULE, ?MODULE).
-define(STACK_PREFIX, "stack").

parse(FileName, Grammar) ->
  parse(FileName, Grammar, []).

parse(FileName, Grammar, Env) ->
  case catch make_tokenizer(FileName, Grammar) of
    {eof, _} ->
      {ok, eof};
    Error = {error, _} ->
      Error;
    Tokenizer ->
      create_stack(),
      lists:foreach(fun ({N, V}) -> var_add(N, V) end, Env),
      Res = (catch yeccpars1([], Tokenizer, 0, [], [])),
      drop_stack(),
      case Res of
	{ok, Result} ->
	  Result;
	Error ->
	  Error
      end
  end.

format_error({Line, Module, Error}) ->
  format_error(io_lib:format("Line ~p: ~s",
			     [Line, Module:format_error(Error)]));
format_error(Message) ->
  case io_lib:deep_char_list(Message) of
    true ->
      lists:flatten(Message);
    _ ->
      io_lib:write(Message)
  end.

% To be used in grammar files to throw an error message to the parser toplevel.
% Doesn't have to be exported!
return_error(Line, Message) ->
  throw({error, {Line, ?THIS_MODULE, Message}}).


% Don't change yeccpars1/6 too much, it is called recursively by yeccpars2/8!
yeccpars1([Token|Tokens], Tokenizer, State, States, Vstack) ->
  yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens,
	    Tokenizer);
yeccpars1([], Tokenizer, State, States,
	  [DE={decl_end, _, _}, {'$INCLUDE', FileName, Line}|Vstack]) ->
  case tokenizer_add(FileName, Line, Tokenizer) of
    {ok, Tokenizer1} ->
      yeccpars1([], Tokenizer1, State, States, [DE, []|Vstack]);
    Error ->
      Error
  end;
yeccpars1([], Tokenizer, State, States, Vstack) ->
  case tokenizer_next(Tokenizer) of
    {eof, Endline} ->
      return_error(Endline, "end_of_file");
    Error = {error, _} ->
      throw(Error);
    {'EXIT', Reason} ->
      return_error(0, Reason);
    {ok, Tokens, Tokenizer1} ->
      case yeccpars1(Tokens, Tokenizer1, State, States, Vstack) of
	error ->
	  Errorline = element(2, hd(Tokens)),
	  return_error(Errorline, "syntax error at or after this line.");
	Other ->
	  Other
      end
  end.

% For internal use only.
yeccerror(Token) ->
    {error,
     {element(2, Token), ?THIS_MODULE,
      ["syntax error before: ", yecctoken2string(Token)]}}.

yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format('~s', [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:format('~w', [A]);
yecctoken2string({Cat, _, Val}) -> io_lib:format('~w', [Val]);

yecctoken2string({'dot', _}) -> io_lib:format('~w', ['.']);
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when atom(Other) ->
    io_lib:format('~w', [Other]);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_simple(Name, Value) when list(Value) ->
  VarName = list_to_atom(Name),
  var_add(VarName, Value),
  {VarName, [Value]};
add_simple(Name, Value) when Value == true; Value == false ->
  {list_to_atom(Name), Value};
add_simple(Name, Value) ->
  {list_to_atom(Name), [Value]}.

add_simple_list(Name, [Value]) ->
  add_simple(Name, Value);
add_simple_list(Name, Values) ->
  {list_to_atom(Name), Values}.

verify_struct({Name, _}, {Name, _}) ->
  ok;
verify_struct({_, LineBegin}, {_, LineEnd}) ->
  return_error(LineEnd, io_lib:format("Tag mismatch at line ~w", [LineBegin])).

add_struct(Name, Attrs, Ctxt) ->
  {list_to_atom(Name), Attrs, Ctxt}.

add_op("INCLUDE", FileName, Line) ->
  {'$INCLUDE', FileName, Line};
add_op(Name, _, Line) ->
  return_error(Line, io_lib:format("Invalid operator ~s", [Name])).

add_value([], List) ->
  List;
add_value(Val, List) ->
  [Val|List].

value_of(Token) when tuple(Token) ->
  element(3, Token);
value_of(Token) ->
   return_error(0, io_lib:format("Bad token ~p", [Token])).

line_of(Token) ->
  element(2, Token).

%% Var Stack
create_stack() ->
  stack_name().

drop_stack() ->
  stack_name() ! stop.

push_frame() ->
  stack_name() ! {push_frame, self()}.

pop_frame() ->
  stack_name() ! {pop_frame, self()}.

var_add(Name, Value) ->
  stack_name() ! {add_var, self(), {Name, Value}}.

var_value(Name) ->
  Self = self(),
  SN = stack_name(),
  SN ! {var_value, Self, Name},
  receive
    {var_value, SN, {Name, Value}} ->
       Value
  end.

stack_name() ->
  SN = list_to_atom(?STACK_PREFIX ++ pid_to_list(self())),
  case whereis(SN) of
    undefined ->
      SP = spawn_link(fun () -> stack_create(self()) end),
      register(SN, SP),
      SP;
    Pid ->
      Pid
  end.
  
stack_create(Parent) ->
  stack_loop({Parent, [stack_new_frame()]}).

stack_loop({Parent, Stack=[Top|Rest]}) ->
  receive
    {push_frame, _} ->
      NF = stack_new_frame(Top),
      stack_loop({Parent, [NF|Stack]});
    {pop_frame, _} ->
      stack_loop({Parent, Rest});
    {add_var, _, {Name, Value}} ->
      Top1 = gb_trees:enter(Name, Value, Top),
      stack_loop({Parent, [Top1|Rest]});
    {var_value, Pid, Name} ->
      Ret =
	case gb_trees:lookup(Name, Top) of
	  none ->
	    "";
	  {value, Val} ->
	    Val
	end,
      Pid ! {var_value, self(), {Name, Ret}},
      stack_loop({Parent, Stack});
    stop ->
      exit(normal);
    {'EXIT', Parent, Reson} ->
      exit(Reson)
  end.

stack_new_frame() ->
  gb_trees:empty().

stack_new_frame(Top) ->
  NF = gb_trees:empty(),
  gb_trees:from_orddict(gb_trees:to_list(Top)).

%% Scanner
make_tokenizer(FileName, Grammar) ->
  case scan_file(FileName, Grammar) of
    [] ->
      {eof, 0};
    Error ={error, _} ->
      Error;
    Tokens ->
      {_, EndLine} = lists:last(Tokens),
      TokFun =
	fun (next, {_, [], EL}) ->
	    {eof, EL};
	    (next, {Fun, [T|Ts], EL}) ->
	    {ok, [T], {Fun, Ts, EL}};
	    ({add, FN, Line}, {Fun, Ts, EL}) ->
	    case catch scan_file(FN, Grammar) of
	      {error, Error} ->
		Msg = io_lib:format("Include file ~s ~s",
				    [FN, format_error(Error)]),
		return_error(Line, Msg);
	      AddTs ->
		AddTs1 = lists:sublist(AddTs, length(AddTs)-1),
		{ok, {Fun, AddTs1 ++ Ts, EL}}
	    end
	end,
      {TokFun, Tokens, EndLine}
  end.

tokenizer_next(State={Fun, _, _}) ->	  
  Fun(next, State).

tokenizer_add(FileName, Line, State={Fun, _, _}) ->
  Fun({add, FileName, Line}, State).

scan_file(FileName, {GrammarMod, Grammar}) ->
  Buf = file_to_buf(FileName),
  AddFName =
    fun ({C, L, V}) -> {C, {FileName, L}, V};
	({C, L}) -> {C, {FileName, L}}
    end,
  GrammarMod:create_env(),
  Res = (catch [AddFName(T) || T <- scan(file_to_buf(FileName), Grammar)]),
  GrammarMod:drop_env(),
  Res.

scan(Buf, Grammar) ->
  case mlex:scan(property_str_buf, Buf, Grammar) of
    Error = {error, _} ->
      throw(Error);
    Res ->
      Res
  end.

file_to_buf(FileName) ->
  case file:read_file(FileName) of
    {ok, Bin} ->
      string_to_buf(Bin);
    Error = {error, _} ->
      throw(Error)
  end.

string_to_buf(Bin) when binary(Bin) ->
  string_to_buf(binary_to_list(Bin));
string_to_buf(Str) ->
  property_str_buf:str_to_buf(Str).

