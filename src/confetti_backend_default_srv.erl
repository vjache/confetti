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

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
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

get_config({subject, ContextPath, Subject}) ->
	AllCtxData=[begin Query = qlc:q([{VarName, VarVal} || 
						  #variable{context = C,
									subject = S, 
									name	 = VarName, 
									value	 = VarVal} 
									   <- mnesia:table(variable), C=:=Context,S=:=Subject]),
		   mnesia:async_dirty(
			 fun()->
					 qlc:eval(Query)
			 end)
	 end || Context <- all_contexts(ContextPath)],
	merge(AllCtxData,undefined);

get_config({subjects, ContextPath, Subjects}) ->
	[{Subject, get_config({subject, ContextPath, Subject}) }||Subject<-Subjects];

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









