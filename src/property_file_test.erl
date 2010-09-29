%%%----------------------------------------------------------------------
%%% File    : property_file_test.erl
%%% Author  :  <svg@surnet.ru>
%%% Purpose : 
%%% Created :  2 Sep 2002 by  <svg@surnet.ru>
%%%----------------------------------------------------------------------
%%%
%%% $Id: property_file_test.erl,v 1.1.1.1 2002/10/04 00:00:00 svg Exp $
%%%
%%% $Log: property_file_test.erl,v $
%%% Revision 1.1.1.1  2002/10/04 00:00:00  svg
%%% Imported sources
%%%
%%% Revision 1.2  2002/10/01 22:32:05  svg
%%% *** empty log message ***
%%%
%%% Revision 1.1.1.1  2002/09/04 20:19:07  svg
%%% Imported sources
%%%
%%%

-module(property_file_test).
-author('svg@surnet.ru').

-export([test_all/0, test_all/1]).
-export([test_apache_parse/1, test_clike_parse/1, test_end_line/1]).

-define(match(_Pat), fun (_Res) ->
                         case _Res of
                           _Pat -> ok;
                           _ -> false
                         end
                     end).
-define(CLIKE_MATCH,
	[{simple_list,["val1","val2","Val3","testvar1"]},
	 {'Quoted Atom',[{193,54,2,1}]},
	 {test_quote_char,["quote string\n\na"]},
	 {ipv4,[{193,54,2,1}]},
	 {netmask1,[{{193,54,2,0},4294967040}]},
	 {netmask2,[{{193,54,2,0},4294967040}]},
	 {prefix,["@prefix@"]},
	 {exec_prefix,["@exec_prefix@"]},
	 {sysconfdir,["@sysconfdir@"]},
	 {localstatedir,["@localstatedir@"]}|_]).

-define(APACHE_MATCH, [{'ServerType',["standalone"]},
		       {'ServerRoot',["/usr/local/psa/apache"]},
		       {'LockFile',["logs/httpd.lock"]},
		       {'PidFile',["logs/httpd.pid"]},
		       {'ScoreBoardFile',["logs/httpd.scoreboard"]},
		       {'ResourceConfig',["conf/srm.conf"]},
		       {'AccessConfig',["conf/access.conf"]},
		       {'ResourceConfig',["/dev/null"]},
		       {'AccessConfig',["/dev/null"]},
		       {'Timeout',[300]},
		       {'KeepAlive',["On"]},
		       {'MaxKeepAliveRequests',[1000]},
		       {'KeepAliveTimeout',[15]},
		       {'MinSpareServers',[5]}|_]).
test_all([quit]) ->
  (catch test_all()),
  halt().

test_all() ->
  lists:foreach(fun(T) ->
                    io:format("~n~s~n",[apply({?MODULE, T}, [info])]),
                    apply({?MODULE, T}, [do])
                end,
                [test_apache_parse, test_clike_parse, test_end_line]
               ).

test_clike_parse(info) ->
  "*** Parse file ***";
test_clike_parse(do) ->
  FileConf = "./priv/test/clike.conf",
  Tests =
    [{"C-like config",
      [FileConf, clike, [{test_var1, "testvar1"}]],
      ?match(?CLIKE_MATCH)}
    ],
  process_tests(fun (F, M, E) -> property_file:parse(F, M, E) end, Tests).

test_apache_parse(info) ->
  "*** Parse file ***";
test_apache_parse(do) ->
  FileConf = "./priv/test/httpd.conf",
  Tests =
    [{"Apache config",
      [FileConf, apache],
      ?match(?APACHE_MATCH)}
    ],
  process_tests(fun (F, M) -> property_file:parse(F, M) end, Tests).

test_end_line(info) ->
  "*** Test Unix, DOS and Mac end lines ***";
test_end_line(do) ->
  Unix = property_file:parse("./priv/test/clike.conf", clike),
  Dos  = property_file:parse("./priv/test/clike.conf.dos", clike),
  Mac  = property_file:parse("./priv/test/clike.conf.mac", clike),
  Tests =
    [
     {"Unix", [Unix, Unix], true},
     {"DOS", [Dos, Unix], true},
     {"Mac", [Mac, Unix], true}
    ],
  process_tests(fun (List1, List2) -> List1 == List2 end, Tests).

%% Test help funs

%process_tests(Tests) ->
%  lists:foreach(fun ({Fun, FunTests}) ->
%                    process_tests(Fun, FunTests)
%                end, Tests).

process_tests(Fun, Tests) ->
  process_tests(Fun, Tests, 1).

%% Test argument is in the form {Msg, Args, TrueResult}
%% Msg        - test purpose
%% Args       - [Arg]
%% TrueResult - true result pattern
process_tests(_Fun, [], _) ->
  ok;
process_tests(Fun, [Test|Tail], Num) ->
  {Msg, Args, ResPat} = Test,
  Status = case catch apply(Fun,Args) of
             Res when is_function(ResPat) ->
               case catch ResPat(Res) of
                 ok ->
                   [passed, ok];
                 _ ->
                   [Res, failed]
               end;
             ResPat ->
               [passed, ok];
             Other ->
               [Other, failed]
           end,
  io:format("Test ~2.2w: "++Msg++" -> ~p, ~p~n", [Num|Status]),
  process_tests(Fun, Tail, Num+1).
