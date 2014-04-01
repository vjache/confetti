%%%-------------------------------------------------------------------
%%% @author <vjache@gmail.com>
%%% @copyright (C) 2011, Vyacheslav Vorobyov.  All Rights Reserved.
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%% http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @doc
%%%     This is an application to make OTP happy. 
%%% @end
%%% Created : Dec 5, 2012
%%%-------------------------------------------------------------------------------
-module(confetti_app).

-behaviour(application).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	 start/2,
	 stop/1
        ]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([ start/0,
	  get_env/1, 
	  get_env/2, 
	  get_conf_root_dir/0, 
	  get_vsn/0, 
	  read_priv_file/1 ]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(APPLICATION, confetti).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

start() ->
    {ok,_} = application:ensure_all_started(confetti).

%% ====================================================================!
%% External functions
%% ====================================================================!
start(_Type, _StartArgs) ->
	case confetti_sup:start_link() of
		{ok, Pid} ->
			start_webserver(),
			{ok, Pid};
		Error ->
			Error
	end.

stop(_State) ->
    ok.

get_env(VarName) ->
    get_env(VarName,fun()-> throw({var_not_configured,VarName}) end).

get_env(VarName,Fallback) ->
    case application:get_env(?APPLICATION,VarName) of
        {ok, Value} -> Value;
        undefined -> 
            if is_function(Fallback) -> Fallback();
               true -> Fallback
            end
    end.

get_conf_root_dir() ->
	confetti_app:get_env(conf_root_dir, "./conf").

get_vsn() ->
	{ok, Vsn} = application:get_key(?APPLICATION, vsn),
	Vsn.

read_priv_file(RelName) ->
	Filename = filename:join(code:priv_dir(?APPLICATION), RelName),
	{ok, Body} = file:read_file(Filename),
	Body.

%% ====================================================================
%% Internal functions
%% ====================================================================

get_http_server_port() ->
	get_env(http_port, 8008).

get_http_server_hostname() ->
	get_env(http_hostname, '_').

get_http_server_pool_size() ->
	get_env(http_poolsize, 1).

start_webserver() ->
    Handlers = [{[<<"/conf/[...]">>], confetti_rest_api_handler, []},
		{[<<"/status">>], confetti_rest_status_handler, []},
		{[<<"/">>], confetti_rest_status_handler, []}],
    Dispatch = cowboy_router:compile(
		 [{get_http_server_hostname(), Handlers}]),
    {ok, _} = cowboy:start_http(
		http, get_http_server_pool_size(), 
		[{port, get_http_server_port()}], 
		[
		 {env, [{dispatch, Dispatch}]}
		]).
