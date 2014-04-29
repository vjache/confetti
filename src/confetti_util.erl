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
%%%     Some common useful functions. 
%%% @end
%%% Created : Dec 27, 2012
%%%-------------------------------------------------------------------------------
-module(confetti_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([to_binary/1, 
		 subst/2,
		 unix_epoch_time_to_universal/1]).

%%
%% API Functions
%%

to_binary(V) when is_atom(V) ->
	atom_to_binary(V, latin1);
to_binary(V) when is_binary(V) ->
	V;
to_binary(V) when is_integer(hd(V)) ->
	list_to_binary(V);
to_binary({utc, {{Y, M, D}, {H, Min, S}}, Mls}) ->
	list_to_binary(
      io_lib:format(
        "~.4.0w-~.2.0w-~.2.0wT~.2.0w:~.2.0w:~.2.0w.~.3.0wZ", 
        [Y,M,D,H,Min,S,Mls])).

subst(Body, VarBinds) when is_binary(Body), tuple_size(hd(VarBinds)) == 2 ->
	lists:foldl(
	  fun({Name, Value}, BodyAcc) ->
			  BName = to_binary(Name),
			  BValue = to_binary(Value),
			  binary:replace(
				BodyAcc, <<"${",BName/binary,"}">>, BValue, [global])
	  end, Body, VarBinds).

unix_epoch_time_to_universal(UnixEpochTimeSeconds) ->
	{{Y, M, D}, {H, Min, S}} = calendar:gregorian_seconds_to_datetime(UnixEpochTimeSeconds),
	{utc, {{Y + 1970, M, D}, {H, Min, S}}, 0}.

%%
%% Local Functions
%%

