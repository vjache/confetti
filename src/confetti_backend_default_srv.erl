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
%%%     This is a server that implements a confetti_backend behaviour 
%%%		in a default way. It serves a requests based on an underlying 
%%%		mnesia data base.
%%% @end
%%% Created : Dec 6, 2012
%%%-------------------------------------------------------------------------------
-module(confetti_backend_default_srv).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("stdlib/include/qlc.hrl").
-include("private.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
	gen_server:start_link({local, default_backend}, ?MODULE, [], []).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok, #state{}}.

handle_call({get_config, Request, Options}=Call, From, State) ->
	spawn_link(
	  fun() ->
		  try ConfData = get_config(Request),
		       gen_server:reply(From, {ok, apply_options(ConfData, Options)})
		  catch 
		      _:Reason ->
			  error_logger:error_report(
			    [{call, Call}, 
			     {reason,Reason}, 
			     {stacktarce, erlang:get_stacktrace()}, 
			     {application, confetti}, 
			     {module, ?MODULE}, 
			     {line,?LINE}]),
			  gen_server:reply(From, {error, Reason})
		  end
	  end),
    {noreply, State};
handle_call({get_status, Options}=Call, From, State) ->
    spawn_link(
      fun() ->
	      try ConfData = get_status(),
		   gen_server:reply(From, {ok, apply_options(ConfData, Options)})
	      catch 
		  _:Reason ->
		      error_logger:error_report(
			[{call, Call}, 
			 {reason,Reason}, 
			 {stacktarce, erlang:get_stacktrace()}, 
			 {application, confetti}, 
			 {module, ?MODULE}, 
			 {line,?LINE}]),
		      gen_server:reply(From, {error, Reason})
	      end
      end),
    {noreply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Re5Aply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

all_contexts(ContextPath) ->
	lists:foldl(
	  fun(C, [])-> [[C]];
	     (C, [L|_]=Acc)-> [L++[C]|Acc] 
	  end, [], ContextPath).

get_all_subjects() ->
	Query = qlc:q([S || #variable{subject = S} <- mnesia:table(variable)]),
	mnesia:async_dirty(
	  fun()->
		  qlc:eval(Query, unique_all)
	  end).

get_status() ->
	Query = qlc:q([SF || SF=#source_file{} <- mnesia:table(source_file)]),
	Data = mnesia:async_dirty(
	  fun()->
		  qlc:eval(Query, unique_all)
	  end),
	Data1 = lists:keysort(#source_file.file, Data),
	[ [{<<"file">>, 		confetti_util:to_binary(F)}, 
	   {<<"parse_status">>, confetti_util:to_binary(S)}, 
	   {<<"timestamp">>, 	confetti_util:to_binary(
		  confetti_util:unix_epoch_time_to_universal(T))}] || #source_file{file = F, parse_status = S, timestamp = T} <- Data1].
	

get_config({subject, ContextPath, Subject}) ->
	AllCtxData =
	[begin Query = qlc:q([{VarName, VarVal} || 
				 #variable{context = C,
					   subject = S, 
					   name	   = VarName, 
					   value   = VarVal} 
				     <- mnesia:table(variable), C=:=Context,S=:=Subject]),
	       mnesia:async_dirty(
		 fun()->
			 qlc:eval(Query)
		 end)
	 end || Context <- all_contexts(ContextPath)],
    merge(AllCtxData,undefined);

get_config({subjects, ContextPath, []}) ->
	[{Subject, get_config({subject, ContextPath, Subject}) }|| Subject <- get_all_subjects()];
get_config({subjects, ContextPath, Subjects}) ->
	[{Subject, get_config({subject, ContextPath, Subject}) }|| Subject <- Subjects];

get_config({subject_variable, ContextPath, Subject, VariableName}) ->
	get_config({subject_variables, ContextPath, Subject, [VariableName]});

get_config({subject_variables, ContextPath, Subject, VariableNames}) ->
	AllCtxData=[collect_var_vals(Ctx, Subject, VariableNames) || Ctx <- all_contexts(ContextPath)],
	merge(AllCtxData, VariableNames).

collect_var_vals(_ContextPath, _Subject, []) ->
	[];
collect_var_vals(ContextPath, Subject, [VariableName | Tail]) ->
	case mnesia:dirty_read(variable,?VAR_ID(ContextPath, Subject, VariableName)) of
		[#variable{value=VarVal}] -> [{VariableName, VarVal} | collect_var_vals(ContextPath, Subject,Tail)];
		[] -> collect_var_vals(ContextPath, Subject,Tail)
	end.

merge(AllCtxData, Keys) ->
	AllCtxDataF = lists:flatten(AllCtxData),
	case Keys of
		undefined ->
			Keys1	 = lists:usort(element(1, lists:unzip(AllCtxDataF))),
			Defaults = [];
		_ -> 
			Keys1 	 = Keys,
			Defaults = [{Key, null} || Key <- Keys]
	end,
	AllCtxDataFD = AllCtxDataF ++ Defaults,
	MergedData = [case lists:keyfind(Key, 1, AllCtxDataFD) of
					  false -> {Key, null};
					  Tup -> Tup
				  end || Key <- Keys1],
	case MergedData of
		[] -> [{}];
		_ -> MergedData
	end.

apply_options(ConfData, _Options) ->
	try
	jsx:prettify(jsx:encode(ConfData))
	catch
		_:Reason ->
			io:format("APPLY-FAILED ~p~n", [{Reason, ConfData}])
	end.









