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
-module(confetti_rest_api_handler).

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
-export([hello_to_html/2]).
-export([hello_to_json/2]).
-export([hello_to_text/2]).

init(_Transport, _Req, []) ->
	io:format("INIT:~p~n", [_Req]),
	{upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
	{[
%% 		{<<"text/html">>, hello_to_html},
		{<<"application/json">>, hello_to_json},
		{<<"text/plain">>, hello_to_text}
	], Req, State}.

hello_to_html(Req, State) ->
	Body = <<"<html>
<head>
	<meta charset=\"utf-8\">
	<title>REST Hello World!</title>
</head>
<body>
	<p>REST Hello World as HTML!</p>
</body>
</html>">>,
	{Body, Req, State}.

% Possible requests are:
%	* /ctx1/ctx2/.../ctxN?subject=SOME_SUBJECT
%	* /ctx1/ctx2/.../ctxN?subject=SOME_SUBJECT&?variables=COMMA_SEPARATED_LIST_OF_VARs
%	* /ctx1/ctx2/.../ctxN?subjects=COMMA_SEPARATED_LIST_OF_SOME_SUBJECTs
% and also flag "&prettify=true" can be add to any above.
hello_to_json(Req, State) ->
	ContextPath = [_|_] = req(path_info, Req),
	case req_qs_val(<<"prettify">>, Req) of
		<<"true">> 	-> Opts = [prettify, json];
		_ 			-> Opts = [json]
	end,
	Subjects 	= req_qs_val(<<"subjects">>, Req),
	JSON = case Subjects of
			   undefined ->
				   Subject 	= req_qs_val(<<"subject">>, Req),
				   Variables 	= req_qs_val(<<"variables">>, Req),
				   case {Subject, Variables} of
					   {undefined, _ } -> % No subject, hence raise error
						   cowboy_req:reply(404,Req),
						   exit(subject_expected);
					   {_, undefined} -> % Subject only, hence all variables of a subject requested
						   confetti_backend:get_subject_config(get_backend(Req), ContextPath, Subject, Opts);
					   {_,_} -> % Subject an variables are defined, hence a subset of variables of a subject requested
						   case binary:split(Variables, <<",">>, [global]) of
							   [Variable] ->
								   confetti_backend:get_subject_variable_config(
									 get_backend(Req), ContextPath, Subject, Variable, Opts);
							   VariableList ->
								   confetti_backend:get_subject_variables_config(
									 get_backend(Req), ContextPath, Subject, VariableList, Opts)
						   end
				   end;
			   _ ->
				   SubjectList = binary:split(Subjects, <<",">>, [global]),
				   confetti_backend:get_subjects_config(
					 get_backend(Req), ContextPath, SubjectList, Opts)
		   end,
	{JSON, Req, State}.

hello_to_text(Req, State) ->
	hello_to_json(Req, State).

%%
%% Local Functions
%%

req(Func,Req) ->
	{V,_}=cowboy_req:Func(Req),
	V.

req_qs_val(Name, Req) ->
	{V,_}=cowboy_req:qs_val(Name, Req),
	V.

get_backend(_Req) ->
	default_backend.

