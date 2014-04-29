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
%%%     This is a cowboy based RESTful HTTP handler. It is designed to expose 
%%%		RESTful HTTP API for simple configuration data access. 
%%% @end
%%% Created : Dec 5, 2012
%%%-------------------------------------------------------------------------------
-module(confetti_rest_status_handler).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%

-export([init/3]).
-export([content_types_provided/2]).
-export([to_json/2, to_text/2]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
	{[
	  {<<"application/json">>, 	to_json },
	  {<<"text/plain">>, 		to_text }
	 ], Req, State}.

% Possible requests are:
%	* /status
to_json(Req, State) ->
	Template = confetti_app:read_priv_file("web/status.json"),
	JSON = confetti_backend:get_status(
		 get_backend(Req), []),
	BVsn = confetti_util:to_binary(
		 confetti_app:get_vsn()),
	VarBinds = [{vsn, <<"\"",BVsn/binary, "\"">>}, 
				{files_statuses, JSON}],
	Body 	 = confetti_util:subst(Template, VarBinds),
	{Body, Req, State}.

to_text(Req, State) ->
	to_json(Req, State).

%%
%% Local Functions
%%

get_backend(_Req) ->
	default_backend.

