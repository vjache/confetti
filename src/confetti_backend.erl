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
%%%     This is a behavior for all possible plugable backends. Backend is a 
%%%		server that may serve a set of confetti specific requests. Mainly 
%%%		requests are quries for application configuration data but also 
%%%		possible some admin requests. 
%%% @end
%%% Created : Dec 5, 2012
%%%-------------------------------------------------------------------------------
-module(confetti_backend).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([get_subject_config/4,
		 get_subject_variable_config/5,
		 get_subject_variables_config/5,
		 get_subjects_config/4]).

%%
%% API Functions
%%

get_subject_config(BackendServerRef, ContextPath, Subject, Options) 
  when is_binary(hd(ContextPath)), 
	   is_binary(Subject) 		->
	validate_reply(
	  gen_server:call(BackendServerRef, {get_config, {subject, ContextPath, Subject}, Options})).

get_subjects_config(BackendServerRef, ContextPath, Subjects, Options) 
  when is_binary(hd(ContextPath)), 
	   is_binary(hd(Subjects)) 	->
	validate_reply(
	  gen_server:call(BackendServerRef, {get_config, {subjects, ContextPath, Subjects}, Options})).

get_subject_variable_config(BackendServerRef, ContextPath, Subject, Variable, Options) 
  when is_binary(hd(ContextPath)), 
	   is_binary(Subject), 
	   is_binary(Variable) 		->
	validate_reply(
	  gen_server:call(BackendServerRef, {get_config, {subject_variable, ContextPath, Subject, Variable}, Options})).

get_subject_variables_config(BackendServerRef, ContextPath, Subject, Variables, Options) 
  when is_binary(hd(ContextPath)), 
	   is_binary(Subject), 
	   is_binary(hd(Variables)) 		->
	validate_reply(
	  gen_server:call(BackendServerRef, {get_config, {subject_variables, ContextPath, Subject, Variables}, Options})).

%%
%% Local Functions
%%

validate_reply({error, Reason}) ->
	throw(Reason);
validate_reply({ok, B}) when is_binary(B) orelse is_binary(hd(B)) ->
	B;
validate_reply(BadReply) ->
	exit({bad_backend_reply, BadReply}).

