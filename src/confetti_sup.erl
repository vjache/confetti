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
%%%     This is a supervisor to make OTP happy. 
%%% @end
%%% Created : Dec 5, 2012
%%%-------------------------------------------------------------------------------
-module(confetti_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

-define(CHILD(I, Type),     {I, {I, start_link, []}, permanent, 15000, Type, [I]}).
-define(CHILD_EVT_MAN(I),   {I, {I, start_link, []}, permanent, 5000, worker, dynamic}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
	Procs = [?CHILD(confetti_backend_default_indexer_srv, worker),
			 ?CHILD(confetti_backend_default_srv, worker)],
	{ok, {{one_for_one, 10, 10}, Procs}}.
