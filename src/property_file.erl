%%%----------------------------------------------------------------------
%%% File    : property_file.erl
%%% Author  :  <svg@surnet.ru>
%%% Purpose : 
%%% Created :  2 Oct 2002 by  <svg@surnet.ru>
%%%----------------------------------------------------------------------

%%
%% $Id: property_file.erl,v 1.1.1.1 2002/10/04 00:00:00 svg Exp $
%%
%% $Log: property_file.erl,v $
%% Revision 1.1.1.1  2002/10/04 00:00:00  svg
%% Imported sources
%%
%%

%% @doc property_file is a configuration file parser.
%% 
%% <p>It supports two popular configuration formats - Apache and C-like,
%% property substitution and file inclusion. It works with Unix, DOS and
%% Mac files. Both formats are line-oriented, declaration may be
%% continued at next line with <em>'\'</em> before new line.</p>
%% 
%% <p>Property substitution may be used in quoted and unquoted string
%% values. If property doesn't exists or its value is not a string, empty
%% string is silently substituted. Syntax - <code>${property_name}</code>.
%% </p>
%% 
%% <h3>C-like Grammar</h3>
%% <pre>
%% config := decls
%%  
%% decls := decl*
%%  
%% decl := bool_decl
%%      |  simple_decl
%%      |  struct_decl
%%      |  include_decl
%% 
%%  
%% bool_decl := property_name : {property_name, true}
%%           |  property_name = bool : {property_name, bool} 
%% 			       
%% bool := 'true' | 'false' | 'yes' | 'no'
%%  
%% simple_decl := property_name '=' simple_value+
%%             : {property_name, [simple_value+]}
%% 
%% struct_decl := property_name property_attr* '{' decl* '}'
%%             : {property_name, [property_attr], [decl]}
%% 
%% property_attr := simple_value
%% 
%% include_decl := '$INCLUDE' string
%% 
%% simple_value := string
%%              |  integer
%%              |  float
%%              |  ipv4_address
%%              |  ipv4_netmask
%% 
%% property_name := squoted_string
%%               | atom_string
%% 
%% string := unquoted_string
%%        |  "double quoted string"
%%        |  'single quoted string'
%% </pre>
%% 
%% <h3>apache-like Grammar</h3>
%% <pre>
%% config := decls
%%  
%% decls := decl*
%%  
%% decl := bool_decl
%%      |  simple_decl
%%      |  struct_decl
%%      |  include_decl
%% 
%%  
%% bool_decl := property_name : {property_name, true}
%%           |  property_name = bool : {property_name, bool} 
%%           |  property_name bool : {property_name, bool} 
%% 			       
%% bool := 'true' | 'false' | 'yes' | 'no'
%%  
%% simple_decl := property_name '=' simple_value+
%%             |  property_name simple_value+
%%             : {property_name, [simple_value+]}
%% 
%% struct_decl := '&lt;'property_name property_attr* '&gt;' decl* '&lt;'/property_name'&gt;'
%%             : {property_name, [property_attr], [decl]}
%% 
%% property_attr := simple_value
%% 
%% include_decl := 'Include' string
%% 
%% simple_value := string
%%              |  integer
%%              |  float
%%              |  ipv4_address
%%              |  ipv4_netmask
%% 
%% property_name := squoted_string
%%               | atom_string
%% 
%% string := unquoted_string
%%        |  "double quoted string"
%%        |  'single quoted string'
%%
%% ipv4_netmask := ipv4 '/' ipv4        : {ipv4, bitmask}
%%              | ipv4 '/' integer_mask : {ipv4, bitmask}
%% 
%% integer_mask =&lt; 32
%%  
%% </pre>
%%
%% @end

-module(property_file).
-author('svg@surnet.ru').

-export([parse/2, parse/3, format_error/1]).

%%@spec parse(FileName::string(), Mode::mode()) -> Result | Error
%%
%%@doc The same as <code>parse/3</code> but with empty initial environment
%%
%%@see parse/3

parse(FileName, Mode) ->
  parse(FileName, Mode, []).

%%@spec parse(FileName::string(), Mode::mode(), Env::env_list()) ->
%% Result | {error, Error}
%%
%% Result = list()
%% mode() = apache | clike
%% env_list() = [{Key, Value}]
%% Key = atom()
%% Value = string()
%% Error = term()
%%
%% @doc Parse file and return result or error. Error can be formatted
%% with <code>format_error/1</code>. Env is a list of parameters which
%% can be substituted in properties values.

parse(FileName, Mode=apache, Env) ->
  apache_property_parse:parse(FileName, property_mlex:grammar(Mode), Env);
parse(FileName, Mode=clike, Env) ->
  clike_property_parse:parse(FileName, property_mlex:grammar(Mode), Env).


%% @spec format_error(Error::term()) -> string()
%% 
%% @doc Format error returned by <code>parse/3</code>

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
  
