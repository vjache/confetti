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
%%%     This is a daemon that scans configured root directory recursively and 
%%%		puts found data into the mnesia. It scans for a "*.json" files, where 
%%%		the name of a file becomes a subject name and a directory (minus 
%%%		configured root) is a context of a subject.
%%% @end
%%% Created : Dec 6, 2012
%%%-------------------------------------------------------------------------------
-module(confetti_backend_default_indexer_srv).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("kernel/include/file.hrl").
-include("private.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {config_dir}).

-define(SERVER, default_backend_indexer).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
	gen_server:start_link({local, ?SERVER},?MODULE, [], []).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
	ConfigDir = confetti_app:get_conf_root_dir(),
	ensure_mnesia_running(),
	ensure_table(
	  variable, [{ram_copies, [node()]},
				 {index, [#variable.context, #variable.subject]},
				 {attributes, record_info(fields,variable)},
				 {storage_properties,
			     [{ets, [compressed]}]}]),
	ensure_table(
	  source_file, [{ram_copies, [node()]},
				 {attributes, record_info(fields,source_file)},
				 {storage_properties,
			     [{ets, [compressed]}]}]),
	self() ! scan_file_system,
	{ok, #state{config_dir=ConfigDir}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

% HANDLE EVENT: Scan Source Files  
handle_info(scan_file_system, #state{config_dir=Dir}=State) ->
	Self = self(),
	UpsertSourceFile = fun(#source_file{}=SourceFile) -> 
							   {atomic,ok} = mnesia:transaction(
											   fun() ->
													   mnesia:write(SourceFile)
											   end),
							   Self ! {parse_source_file, SourceFile}
					   end, 
	AllFilesFound = 
		filelib:fold_files(
		  Dir, ".*\.json$", true, 
		  fun(File, FilesFound)->
				  {ok, #file_info{ctime=CTime}}=file:read_file_info(File, [{time,posix}]),
				  case mnesia:dirty_read(source_file, File) of
					  [#source_file{timestamp=Timestamp}=SF] ->
						  if CTime > Timestamp ->
								 UpsertSourceFile(SF);
							 true -> 
								 ok
						  end;
					  [] ->
						  UpsertSourceFile(#source_file{file=File, timestamp=CTime})
				  end,
				  [File | FilesFound]
		  end, []),
	AllFilesFoundSet = sets:from_list(AllFilesFound),
	AllSourceFilesTobeDeleted  = 
		mnesia:async_dirty(
		  fun()->
				  mnesia:foldl(
					fun(#source_file{file=File}=SF, SourceFilesTobeDeleted)->
							case sets:is_element(File, AllFilesFoundSet) of
								true -> SourceFilesTobeDeleted;
								false ->
									[SF | SourceFilesTobeDeleted]
							end
					end, [], source_file)
		  end),
	case AllSourceFilesTobeDeleted of
		[] -> ok;
		_  -> 
			self() ! {cleanup_source_files, AllSourceFilesTobeDeleted}
	end,
	erlang:send_after(60000, self(), scan_file_system),
	{noreply, State};

% HANDLE EVENT: Parse Source File
handle_info({parse_source_file, SrcFile = #source_file{ file = File } }, #state{config_dir=Dir}=State) ->
	% 1. Parse file name to extract context & subject
	{Context, Subject} 	= parse_source_filename(Dir, File),
	% 2. Read source file
	{ok, Body}			= file:read_file(File),
	% 3. Parse JSON file
	try json_decode_lined(Body) of
		JSONObj when tuple_size(hd(JSONObj)) == 2 ->
			transaction(
			  fun() ->
					  % 4. Clean up data
					  clean_variables_by_origin(File),
					  % 5. Write variables
					  [begin
						   Var = #variable{id 	   	 = ?VAR_ID(Context, Subject, VarName),
										   context 	 = Context,
										   subject 	 = Subject, 
										   name	   	 = VarName,
										   value   	 = VarValue,
										   origin  	 = File},
						   mnesia:write(Var)
					   end || {VarName, VarValue} <- JSONObj],
					  % 6. Set parse status 'ok'
					  mnesia:write(SrcFile#source_file{parse_status=ok}),
					  ok
			  end),
			ok;
		[{}] ->
			transaction(
			  fun() ->
					  mnesia:write(SrcFile#source_file{parse_status=ok}),	ok
			  end)
	catch
		_:{json_parse_error, Reason, LineNo, Line} ->
			Status = {json_parse_error, 
					  [{reason, Reason}, 
					   {line_no, LineNo}, 
					   {line, Line}, 
					   {file, File}]},
			transaction(
			  fun() ->
					  mnesia:write(SrcFile#source_file{parse_status=Status}),	ok
			  end)
	end,
	{noreply, State};

% HANDLE EVENT: Clean Source File Entry At Mnesia
handle_info({cleanup_source_files, AllSourceFilesTobeDeleted}, #state{}=State) ->
	[begin
		 transaction(
		   fun() ->
				   mnesia:delete_object(SF),
				   clean_variables_by_origin(File)
		   end)
	 end || #source_file{file=File}=SF <- AllSourceFilesTobeDeleted],
	{noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

clean_variables_by_origin(Origin) ->
	VarsToDel = mnesia:match_object(#variable{origin=Origin, _ = '_'}),
	[mnesia:delete_object(V) || V <- VarsToDel],
	ok.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

transaction(Fun) ->
	{atomic,Val} = mnesia:transaction(Fun),
	Val.

-spec parse_source_filename(Dir :: string(), SourceFilename :: string()) -> {Context :: [binary()], Subject :: binary()}.
parse_source_filename(Dir, SourceFilename) ->
	NSkip 			= length(filename:split(Dir)),
	File1 			= lists:nthtail(NSkip, filename:split(SourceFilename)),
	Context 		= [ list_to_binary(S) || S <- lists:sublist(File1, length(File1) - 1)],
	Subject 		= list_to_binary(filename:basename(lists:last(File1), ".json")),
	{Context, Subject}.

ensure_mnesia_running() ->
    case mnesia:system_info(is_running) of
        yes -> ok;
        no -> throw(mnesia_not_running)
    end.

ensure_table(TableName,TableAttrs) ->
    case mnesia:create_table(
           TableName,
           TableAttrs) of
        {atomic, ok} ->
            ok=mnesia:set_master_nodes(TableName, [node()]),
            ok;
        {aborted, {already_exists, TableName}} ->
            ok;
        {aborted, Reason} ->
            throw(Reason)
    end.

json_decode_lined(Body) ->
	Lines = binary:split(Body, [<<"\n">>, <<"\r">>], [global]),
	json_decode_lined(Lines, {incomplete, fun jsx:decode/1}, 0).

json_decode_lined([], Result, _N) ->
	case Result of
		{incomplete, _F} 	-> exit(json_incomplete);
		_ 					-> Result
	end;
json_decode_lined([H|T], {incomplete, F}, N) when is_binary(H) ->
	try F(H) of
		Cont ->
			json_decode_lined(T, Cont, N + 1)
	catch
		_:Reason ->
			exit({json_parse_error, Reason, N, H})
	end.