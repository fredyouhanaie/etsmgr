%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2019, Fred Youhanaie
%%% @doc
%%%
%%% The main `etsmgr' server (gen_server).
%%%
%%% @end
%%% Created :  1 Apr 2019 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(etsmgr_srv).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([new_table/4, add_table/3, del_table/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {tables}).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc Create and manage an ETS table.
%%
%% This functions is called from the `etsmgr' module.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_table(atom(), atom(), atom(), list()) -> {ok, pid(), ets:tid()} | {error, term()}.
new_table(Inst_name, Table_name, ETS_name, ETS_opts) ->
    Server_name = etsmgr:inst_to_name(?SERVER, Inst_name),
    case gen_server:call(Server_name, {new_table, Table_name, ETS_name, ETS_opts}) of
	{ok, Mgr_pid, Table_id} ->
	    try ets:setopts(Table_id, {heir, Mgr_pid, Table_name}) of
		true ->
		    {ok, Mgr_pid, Table_id}
	    catch
		error:E ->
		    {error, E}
	    end;
	Error ->
	    Error
    end.


%%--------------------------------------------------------------------
%% @doc manage an existing ETS table.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_table(atom(), atom(), ets:tid()) -> {ok, pid(), ets:tid()} | {error, term()}.
add_table(Inst_name, Table_name, Table_id) ->
    Server_name = etsmgr:inst_to_name(?SERVER, Inst_name),
    case gen_server:call(Server_name, {add_table, Table_name, Table_id}) of
	{ok, Mgr_pid, Table_id} ->
	    try ets:setopts(Table_id, {heir, Mgr_pid, Table_name}) of
		true ->
		    {ok, Mgr_pid, Table_id}
	    catch
		error:E ->
		    {error, E}
	    end;
	Error ->
	    Error
    end.


%%--------------------------------------------------------------------
%% @doc Remove a table from the list of managed tables.
%%
%% @end
%%--------------------------------------------------------------------
-spec del_table(atom(), atom()) -> {ok, pid()} | {error, term()}.
del_table(Inst_name, Table_name) ->
    Server_name = etsmgr:inst_to_name(?SERVER, Inst_name),
    gen_server:call(Server_name, {del_table, Table_name}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
		      {error, Error :: {already_started, pid()}} |
		      {error, Error :: term()} |
		      ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
			      {ok, State :: term(), Timeout :: timeout()} |
			      {ok, State :: term(), hibernate} |
			      {stop, Reason :: term()} |
			      ignore.
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{tables=maps:new()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
			 {reply, Reply :: term(), NewState :: term()} |
			 {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
			 {reply, Reply :: term(), NewState :: term(), hibernate} |
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
			 {stop, Reason :: term(), NewState :: term()}.

handle_call({new_table, Table_name, ETS_name, ETS_opts}, _From={Cli_pid, _Cli_ref}, State) ->
    case handle_new_table(Table_name, ETS_name, ETS_opts, Cli_pid, State#state.tables) of
	{ok, Mgr_pid, Table_id, Tables} ->
	    {reply, {ok, Mgr_pid, Table_id}, #state{tables=Tables}};
	Error = {error, _Reason} ->
	    {reply, Error, State}
    end;

handle_call({add_table, Table_name, Table_id}, _From={Cli_pid, _Cli_ref}, State) ->
    case handle_add_table(Table_name, Table_id, Cli_pid, State#state.tables) of
	{ok, Mgr_pid, Table_id, Tables} ->
	    {reply, {ok, Mgr_pid, Table_id},  #state{tables=Tables}};
	Error = {error, _Reason} ->
	    {reply, Error, State}
    end;

handle_call({del_table, Table_name}, _From={Cli_pid, _Cli_ref}, State) ->
    case handle_del_table(Table_name, Cli_pid, State#state.tables) of
	{ok, Mgr_pid, Tables} ->
	    {reply, {ok, Mgr_pid}, #state{tables=Tables}};
	Error = {error, _Reason} ->
	    {reply, Error, State}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: normal | term(), NewState :: term()}.
handle_info({'EXIT', Cli_pid, Reason}, State) ->
    Tables = State#state.tables,
    {ok, Tables2} = handle_EXIT(Cli_pid, Reason, Tables),
    {noreply, #state{tables=Tables2}};

handle_info({'ETS-TRANSFER', Table_id, Cli_pid, Heir_data}, State) ->
    {ok, Tables2} = handle_ETS_TRANSFER(Table_id, Cli_pid, Heir_data, State#state.tables),
    {noreply, #state{tables=Tables2}};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
				      {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Handle the `new_table' request.
%%
%% <ol>
%%
%% <li>Check, and, if needed, create ETS table</li>
%%
%% <li>If all ok, return the etsmgr pid, the table id' and and updates
%% tables map.</li>
%%
%% </ol>
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_new_table(atom(), atom(), list(), pid(), map()) -> {ok, pid(), ets:tid(), map()} | {error, term()}.
handle_new_table(Table_name, ETS_name, ETS_opts, Cli_pid, Tables) ->
    Result = case maps:find(Table_name, Tables) of
		 error -> %% error is good, we create new table and entry
		     new_table_ets(ETS_name, ETS_opts, Cli_pid);
		 {ok, Table} ->
		     {ok, Table_tid} = maps:find(tabid, Table),
		     case ets:info(Table_tid) of
			 undefined -> %% dead table entry, we create new table and entry
			     new_table_ets(ETS_name, ETS_opts, Cli_pid);
			 _ -> %% We have a live entry, check/update entry
			     new_table_entry(Table_tid, Cli_pid)
		     end
	     end,
    case Result of
	{ok, Mgr_pid, Table_id, Table2} ->
	    {ok, Mgr_pid, Table_id, maps:put(Table_name, Table2, Tables)};
	Error = {error, _Reason} ->
	    Error
    end.


%%--------------------------------------------------------------------
%% @doc Create an ETS table and manage it.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_table_ets(atom(), list(), pid()) -> {ok, pid(), ets:tid(), map()} | {error, term()}.
new_table_ets(ETS_name, ETS_opts, Cli_pid) ->
    try ets:new(ETS_name, ETS_opts) of
	ETS_table ->
	    Table_id = ets:info(ETS_table, id),
	    new_table_entry(Table_id, Cli_pid)
    catch
	error:E ->
	    {error, E}
    end.


%%--------------------------------------------------------------------
%% @doc Create/Update table entry for an existing ETS table.
%%
%% The request is rejected, if the caller is neither the owner of the
%% table, nor the registered client.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_table_entry(ets:tid(), pid()) -> {ok, pid(), ets:tid(), map()} | {error, term()}.
new_table_entry(Table_id, Cli_pid) ->
    Mgr_pid = self(),
    case ets:info(Table_id, owner) of
	Mgr_pid ->
	    ets:give_away(Table_id, Cli_pid, etsmgr),
	    link(Cli_pid),
	    {ok, Mgr_pid, Table_id, #{tabid => Table_id, clipid => Cli_pid}};
	Cli_pid ->
	    link(Cli_pid),
	    {ok, Mgr_pid, Table_id, #{tabid => Table_id, clipid => Cli_pid}};
	_Other_owner ->
	    {error, table_exists}
    end.


%%--------------------------------------------------------------------
%% @doc handle the `add_table' request.
%%
%% Create a new table entry, however, if the `Table_name' already
%% exists, we will perform further checks.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_add_table(atom(), ets:tid(), pid(), map()) -> {ok, pid(), ets:tid(), map()} | {error, term()}.
handle_add_table(Table_name, Table_id, Cli_pid, Tables) ->
    ETS_tabid = ets:info(Table_id, id),
    Result = case maps:find(Table_name, Tables) of
	error -> %% i.e. no table entry
	    check_table_ets_entry(ETS_tabid, Cli_pid);
	{ok, Table} ->
	    check_table_entry(Table, ETS_tabid, Cli_pid)
    end,
    case Result of
	{ok, Mgr_pid, ETS_tabid, Table2} ->
	    {ok, Mgr_pid, ETS_tabid, maps:put(Table_name, Table2, Tables)};
	Error = {error, _Reason} ->
	    Error
    end.


%%--------------------------------------------------------------------
%% @doc Check the status of an ETS table, and create/update table entry.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_table_ets_entry(ets:tid(), pid()) ->  {ok, pid(), ets:tid(), map()} | {error, term()}.
check_table_ets_entry(Table_id, Cli_pid) ->
    Mgr_pid = self(),
    case ets:info(Table_id, owner) of
	Mgr_pid ->
	    new_table_entry(Table_id, Cli_pid);
	Cli_pid ->
	    new_table_entry(Table_id, Cli_pid);
	_Other_owner ->
	    {error, not_owner}
    end.


%%--------------------------------------------------------------------
%% @doc Chec, then create/update a table entry.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_table_entry(map(), ets:tid(), pid()) -> {ok, pid(), ets:tid(), map()} | {error, term()}.
check_table_entry(Table, Table_id, Cli_pid) ->
    {ok, Table_tabid} = maps:find(tabid, Table),
    {ok, Table_clipid} = maps:find(clipid, Table),
    case ets:info(Table_tabid) of
	undefined -> %% we have a dead entry
	    unlink(Table_clipid),
	    new_table_entry(Table_id, Cli_pid);
	_ ->
	    if
		Table_id =/= Table_tabid ->
		    {error, table_exists};
		true ->
		    Mgr_pid = self(),
		    case ets:info(Table_id, owner) of
			Mgr_pid ->
			    unlink(Table_clipid),
			    new_table_entry(Table_id, Cli_pid);
			Cli_pid ->
			    unlink(Table_clipid),
			    new_table_entry(Table_id, Cli_pid);
			_Other_owner ->
			    {error, not_owner}
		    end
	    end
    end.


%%--------------------------------------------------------------------
%% @doc Handle a del_table request.
%%
%% The table entry for `Table_name' is removed, and the client is
%% unlinked. It is up to the calling client to delete the ETS table,
%% even if the `etsmgr' is the owner of the ETS table. In this case
%% the client can request ownership via `add_table', before calling
%% `del_table'.
%%
%% A number of checks are performed before taking any action:
%%
%% <ol>
%%
%% <li>If the table entry does not exist, error is returned.</li>
%%
%% <li>If the calling client is neither the owner of the table, nor
%% the registered client, then error is returned.</li>
%%
%% <li>Otherwise, the registered client is unlinked and the table
%% entry is removed.</li>
%%
%% <li>Since the client is expected to be the owner, it is responsible
%% for deleting or removing `etsmgr' as the heir.</li>
%%
%% </ol>
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_del_table(atom(), pid(), map()) -> {ok, pid(), map()} | {error, term()}.
handle_del_table(Table_name, Cli_pid, Tables) ->
    case maps:get(Table_name, Tables) of
	error ->
	    {error, no_such_table_entry};
	Table ->
	    case del_table_check(Cli_pid, Table) of
		{ok, Table_clipid} ->
		    unlink(Table_clipid),
		    Mgr_pid = self(),
		    {ok, Mgr_pid, maps:remove(Table_name, Tables)};
		Error ->
		    Error
	    end
    end.


%%--------------------------------------------------------------------
%% @doc Check a table entry before deleting it.
%%
%% We can only remove an entry if the request comes from the
%% registered client, or from the owner of the table.
%%
%% @end
%%--------------------------------------------------------------------
-spec del_table_check(pid(), map()) -> {ok, pid()} | {error, term()}.
del_table_check(Cli_pid, Table) ->
    Table_tabid = maps:get(tabid, Table),
    Table_clipid = maps:get(clipid, Table),
    if
	Cli_pid == Table_clipid ->
	    {ok, Table_clipid};
	true ->
	    case ets:info(Table_tabid, owner) of
		undefined ->
		    {error, no_such_ets_table};
		Table_clipid ->
		    {ok, Table_clipid};
		_Other_owner ->
		    {error, not_owner}
	    end
    end.


%%--------------------------------------------------------------------
%% @doc handle the `EXIT' message.
%%
%% We scan through existing tables, remove `Cli_pid' from the matching
%% entries.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_EXIT(pid(), term(), map()) -> {ok, map()}.
handle_EXIT(Cli_pid, _Reason, Tables) ->
    Tables2 = maps:map(
		fun (_Table_name, Table) ->
			maps:map(
			  fun (clipid, Table_clipid)
				when Table_clipid == Cli_pid ->
				  none;
			      (_Key, Value) ->
				  Value
			  end,
			  Table
			 )
		end,
		Tables
	       ),
    {ok, Tables2}.


%%--------------------------------------------------------------------
%% @doc Handle the `ETS-TRANSFER' message.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_ETS_TRANSFER(ets:tid(), pid(), term(), map()) -> {ok, map()}.
handle_ETS_TRANSFER(_Table_id, _Cli_pid, _Heir_data, Tables) ->
    {ok, Tables}.
