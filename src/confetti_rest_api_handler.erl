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
%% API Functions
%%

-export([init/3,
		 content_types_provided/2,
		 to_json/2, 
		 to_text/2]).

-record(args, {subjects, subject, variables, prettify}).
-define(UNDEF, undefined).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, to_json},
		{<<"text/plain">>, to_text}
	], Req, State}.

% Possible requests are:
%	* /ctx1/ctx2/.../ctxN?subject=SOME_SUBJECT
%	* /ctx1/ctx2/.../ctxN?subject=SOME_SUBJECT&?variables=COMMA_SEPARATED_LIST_OF_VARs
%	* /ctx1/ctx2/.../ctxN?subjects=COMMA_SEPARATED_LIST_OF_SOME_SUBJECTs
% and also flag "&prettify=true" can be add to any above.
to_json(Req, State) ->
    try
	ContextPath = [_|_] = req(path_info, Req),
	Args = parse_args(Req),
	Opts = opts(Args, [json]),
	JSON = to_json(Req, ContextPath, Args, Opts, State),
	{JSON, Req, State}
    catch
	_:bad_arg_comb ->
	    cowboy_req:reply(404,Req);
	_:{unexpected_arguments, _ListUnexpected} ->
	    cowboy_req:reply(404,Req)
    end.

to_json(_Req, _ContextPath, 
	#args{subject   = Subject, 
	      variables = Variables, 
	      subjects  = Subjects}, 
	_Opts, _State)
  when Subjects =/= ?UNDEF andalso 
       (Subject =/= ?UNDEF orelse Variables =/= ?UNDEF) ->
    exit(bad_arg_comb);
to_json(Req, ContextPath, 
	#args{subject   = Subject, 
	      variables = ?UNDEF, 
	      subjects  = ?UNDEF}, 
	Opts, _State) 
  when Subject =/= ?UNDEF ->
    confetti_backend:get_subject_config(get_backend(Req), ContextPath, Subject, Opts);
to_json(Req, ContextPath, 
	#args{subject   = Subject, 
	      variables = Variables, 
	      subjects  = ?UNDEF}, 
	Opts, _State) 
  when Subject =/= ?UNDEF andalso 
       Variables =/= ?UNDEF ->
    case split(Variables) of
	[Variable] ->
	    confetti_backend:get_subject_variable_config(
	      get_backend(Req), ContextPath, Subject, Variable, Opts);
	VariableList ->
	    confetti_backend:get_subject_variables_config(
	      get_backend(Req), ContextPath, Subject, VariableList, Opts)
    end;
to_json(Req, ContextPath, 
	#args{subject   = ?UNDEF, 
	      variables = ?UNDEF, 
	      subjects  = Subjects}, 
	Opts, _State) ->
    SubjectList = case Subjects of 
		      ?UNDEF -> [];
		      _ 	 -> split(Subjects)
		  end,
    confetti_backend:get_subjects_config(
      get_backend(Req), ContextPath, SubjectList, Opts).

to_text(Req, State) ->
    to_json(Req, State).

%%
%% Local Functions
%%

parse_args(Req) ->
	{List,_}=cowboy_req:qs_vals(Req),
	case lists:foldl(
		   fun(K, {Args, L, N})->
				   KB = atom_to_binary(K, latin1),
				   case lists:keytake(KB, 1, L) of
					   {value, {_, Value}, L1} ->
						   {setelement(N, Args, Value), L1, N + 1};
					   false ->
						   {Args, L, N + 1}
				   end
		   end, {#args{}, List, 2}, record_info(fields, args)) of
		{Args, [], _} ->
			Args;
		{_Args, ListUnexpected, _} ->
			exit({unexpected_arguments, ListUnexpected})
	end.

req(Func,Req) ->
	{V,_}=cowboy_req:Func(Req),
	V.

get_backend(_Req) ->
	default_backend.

split(Binary) ->
	binary:split(Binary, <<",">>, [global]).

opts(#args{prettify = Prettify}, Opts0) when Prettify orelse Prettify == <<"true">> ->
	[ prettify | Opts0];
opts(_Args, Opts0) ->
	Opts0.

