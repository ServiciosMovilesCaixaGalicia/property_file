%%%----------------------------------------------------------------------
%%% File    : property_str_buf.erl
%%% Author  :  <svg@surnet.ru>
%%% Purpose : Property string buffer
%%% Created :  2 Sep 2002 by  <svg@surnet.ru>
%%%----------------------------------------------------------------------
%%%
%%% $Id: property_str_buf.erl,v 1.1.1.1 2002/10/04 00:00:00 svg Exp $
%%%
%%% $Log: property_str_buf.erl,v $
%%% Revision 1.1.1.1  2002/10/04 00:00:00  svg
%%% Imported sources
%%%
%%% Revision 1.1.1.1  2002/09/04 20:19:07  svg
%%% Imported sources
%%%
%%%

-module(property_str_buf).
-author('svg@surnet.ru').

-export([forward/1, nxtchr/1, lnum/1]).
-export([ str_to_buf/1]).

-record(buf, {str=[],  pos=0, lnum=0}).

str_to_buf(Str) when is_list(Str) ->
  #buf{str=Str, lnum=0}.

nxtchr(B=#buf{pos=0}) ->
  {sof, B#buf{pos=start}};
nxtchr(B=#buf{str=[]}) ->
  {eof, B};
nxtchr(B=#buf{str=[C=$\r,$\n|Cs], lnum=_LN}) ->
  {C, B#buf{str=[$\n|Cs]}};
nxtchr(B=#buf{str=[$\\, EOL], lnum=LN}) when EOL == $\r; EOL == $\n ->
  {eof, B#buf{str=[], lnum=LN+1}};
nxtchr(B=#buf{str=[$\\, EOL, C|Cs], lnum=LN}) when EOL == $\r; EOL == $\n ->
  {C, B#buf{str=Cs, lnum=LN+1}};
nxtchr(B=#buf{str=[C|Cs], lnum=LN}) when C == $\r; C == $\n ->
  {C, B#buf{str=Cs, lnum=LN+1}};
nxtchr(B=#buf{str=[C|Cs]}) ->
  {C, B#buf{str=Cs}}.

lnum(_B=#buf{lnum=LN}) ->
  LN.

forward(B) ->
  B.
