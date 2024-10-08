% -*- indent-tabs-mode:nil; -*-
%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2019, Fred Youhanaie
%%% @doc
%%%
%%% This is the main `etsmgr' server (gen_server). It can manage
%%% multiple ETS tables from multiple clients.
%%%
%%% The server is expected to be supervised either by an instance of
%%% `etsmgr_sup', or a client's own supervisor.
%%%
%%% All client requests to this server are expected to come from
%%% functions in the `etsmgr' module.
%%%
%%% The server maintains an internal `map' of the ETS tables that it
%%% has been asked to manage. Each entry in this map is identified by
%%% a unique key supplied by the client, and contains a further map of
%%% details of the ETS table.
%%%
%%% As well as the tables map above, we also maintain a map of client
%%% pids, where we keep count of the number of tables each client
%%% has. The count is used to ensure we do not unlink a client process
%%% until we've received `del_table' for all of the client's tables.
%%%
%%% New entries are added via the `new_table' or `add_table' requests.
%%%
%%% Existing entries are removed via the `del_table' request.
%%%
%%% If the owner process of a table terminates, the server will
%%% receive an `EXIT' message. On receiving the message the entries
%%% for all tables owned by that process will be flagged as having no
%%% client owner.
%%%
%%% @end
%%% Created :  1 Apr 2019 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(etsmgr_srv).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([new_table/4, add_table/3, del_table/2, info/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {inst_name, tables, clients}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Create and manage an ETS table.
%%
%% This functions is expected to be called from the `etsmgr' module.
%%
%% If `TableName' is not known to `etsmgr', a new ETS table will be
%% created, the calling process will be the owner of the table, with
%% `etsmgr' the heir, and `etsmgr' will be linked to the client
%% process.
%%
%% If an entry for `TableName' already exists, then either the client
%% has restarted following a crash, or there is a name conflict with
%% another application. In the case of a restart, `etsmgr' will behave
%% as if this is a fresh start, but the table will not be created and
%% no data will be lost. In the case of a name conflict, an error will
%% be returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_table(atom(), atom(), atom(), list()) -> {ok, pid(), ets:tid()} | {error, term()}.
new_table(InstName, TableName, ETSname, ETSopts) ->
    ServerName = etsmgr:inst_to_name(?SERVER, InstName),
    case gen_server:call(ServerName, {new_table, TableName, ETSname, ETSopts}) of
        {ok, MgrPid, TableId} ->
            try ets:setopts(TableId, {heir, MgrPid, TableName}) of
                true ->
                    {ok, MgrPid, TableId}
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
%% This functions is expected to be called from the `etsmgr' module.
%%
%% This function is typically called when an application needs
%% `etsmgr' to manage its ETS table(s), which already exist, either
%% because the application prefers to create its own tables, or it has
%% detected that the instance of `etsmgr' that was already managing
%% the ETS tables has crashed and restarted, and this server is the
%% replacement instance.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_table(atom(), atom(), ets:tid()) -> {ok, pid(), ets:tid()} | {error, term()}.
add_table(InstName, TableName, TableId) ->
    ServerName = etsmgr:inst_to_name(?SERVER, InstName),
    case gen_server:call(ServerName, {add_table, TableName, TableId}) of
        {ok, MgrPid, TableId} ->
            try ets:setopts(TableId, {heir, MgrPid, TableName}) of
                true ->
                    {ok, MgrPid, TableId}
            catch
                error:E ->
                    {error, E}
            end;
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Remove a table from the list of managed tables, and unlink
%% from the client_pid.
%%
%% We can only remove an entry if the request comes from the
%% registered client, or from the owner of the table. If this
%% condition is not met, `{error, not_owner}' is returned.
%%
%% We will also remove a table entry if the ETS table no longer
%% exists.
%%
%% Since the client is expected to be the owner, it is responsible for
%% deleting or removing `etsmgr' as the heir.
%%
%% @end
%%--------------------------------------------------------------------
-spec del_table(atom(), atom()) -> {ok, pid()} | {error, term()}.
del_table(InstName, TableName) ->
    ServerName = etsmgr:inst_to_name(?SERVER, InstName),
    gen_server:call(ServerName, {del_table, TableName}).

%%--------------------------------------------------------------------
%% @doc Return the tables currently under management.
%%
%% This is primarily for debugging and troubleshooting.
%%
%% A map is returned where each entry has the client supplied
%% `TableName' as key, and a further map as the value. The inner map
%% contains details of the corresponding ETS table being managed.
%%
%% @end
%%--------------------------------------------------------------------
-spec info(atom()) -> map().
info(InstName) ->
    ServerName = etsmgr:inst_to_name(?SERVER, InstName),
    gen_server:call(ServerName, {info}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom()) -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore.
start_link(InstName) ->
    ServerName = etsmgr:inst_to_name(?SERVER, InstName),
    gen_server:start_link({local, ServerName}, ?MODULE, InstName, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(atom()) -> {ok, State :: term()} |
                              {ok, State :: term(), Timeout :: timeout()} |
                              {ok, State :: term(), hibernate} |
                              {stop, Reason :: term()} |
                              ignore.
init(InstName) ->
    process_flag(trap_exit, true),
    {ok, #state{inst_name=InstName, tables=maps:new(), clients=maps:new()}}.

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

handle_call({new_table, TableName, ETSname, ETSopts}, {CliPid, _CliRef}=_From, State) ->
    case handle_new_table(TableName, ETSname, ETSopts, CliPid,
                          State#state.tables, State#state.clients) of
        {ok, MgrPid, TableId, Tables, Clients} ->
            {reply, {ok, MgrPid, TableId}, State#state{tables=Tables, clients=Clients}};
        Error = {error, _Reason} ->
            {reply, Error, State}
    end;

handle_call({add_table, TableName, TableId}, {CliPid, _CliRef}=_From, State) ->
    case handle_add_table(TableName, TableId, CliPid, State#state.tables, State#state.clients) of
        {ok, MgrPid, TableId, Tables, Clients} ->
            {reply, {ok, MgrPid, TableId},  State#state{tables=Tables, clients=Clients}};
        Error = {error, _Reason} ->
            {reply, Error, State}
    end;

handle_call({del_table, TableName}, {CliPid, _CliRef}=_From, State) ->
    case handle_del_table(TableName, CliPid, State#state.tables, State#state.clients) of
        {ok, MgrPid, Tables, Clients} ->
            {reply, {ok, MgrPid}, State#state{tables=Tables, clients=Clients}};
        Error = {error, _Reason} ->
            {reply, Error, State}
    end;

handle_call({info}, _From, State) ->
    {reply, State#state.tables, State};

handle_call(Request, From, State) ->
    logger:warning("~p:handle_call: Unexpected request=~p, from pid=~p, ignored.",
                   [?SERVER, Request, From]),
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
handle_cast(Request, State) ->
    logger:warning("~p:handle_cast: Unexpected request=~p, ignored.",
                   [?SERVER, Request]),
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
handle_info({'EXIT', CliPid, Reason}, State) ->
    Tables = State#state.tables,
    Clients = State#state.clients,
    {ok, Tables2, Clients2} = handle_exit(CliPid, Reason, Tables, Clients),
    {noreply, State#state{tables=Tables2, clients=Clients2}};

handle_info({'ETS-TRANSFER', TableId, CliPid, HeirData}, State) ->
    {ok, Tables2} = handle_ets_transfer(TableId, CliPid, HeirData, State#state.tables),
    {noreply, State#state{tables=Tables2}};

handle_info(Info, State) ->
    logger:warning("~p:handle_info: Unexpected message=~p.", [?SERVER, Info]),
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
terminate(Reason, State) ->
    logger:info("~p:terminate: reason=~p, state=~p.", [?SERVER, Reason, State]),
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
%% <li>If all ok, return the etsmgr pid, the table id, and update the
%% tables map.</li>
%%
%% </ol>
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_new_table(atom(), atom(), list(), pid(), map(), map())
                      -> {ok, pid(), ets:tid(), map(), map()} | {error, term()}.
handle_new_table(TableName, ETSname, ETSopts, CliPid, Tables, Clients) ->
    logger:info("~p:handle_new_table: TableName=~p, ETSname=~p, ETSopts=~p, CliPid=~p.",
                [?SERVER, TableName, ETSname, ETSopts, CliPid]),
    Result = case maps:find(TableName, Tables) of
                 error -> %% error is good, we create new table and entry
                     new_table_ets(ETSname, ETSopts, CliPid, Clients);
                 {ok, Table} ->
                     {ok, TableTid} = maps:find(tabid, Table),
                     case ets:info(TableTid) of
                         undefined -> %% dead table entry, we create new table and entry
                             new_table_ets(ETSname, ETSopts, CliPid, Clients);
                         _ -> %% We have a live entry, check/update entry
                             new_table_entry(TableTid, CliPid, Clients)
                     end
             end,

    case Result of
        {ok, MgrPid, TableId, Table2, Clients2} ->
            {ok, MgrPid, TableId, maps:put(TableName, Table2, Tables), Clients2};
        Error = {error, _Reason} ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Create an ETS table and manage it.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_table_ets(atom(), list(), pid(), map())
                   -> {ok, pid(), ets:tid(), map(), map()} | {error, term()}.
new_table_ets(ETSname, ETSopts, CliPid, Clients) ->
    try ets:new(ETSname, ETSopts) of
        ETStable ->
            TableId = ets:info(ETStable, id),
            new_table_entry(TableId, CliPid, Clients)
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
-spec new_table_entry(ets:tid(), pid(), map())
                     -> {ok, pid(), ets:tid(), map(), map()} | {error, term()}.
new_table_entry(TableId, CliPid, Clients) ->
    MgrPid = self(),
    case ets:info(TableId, owner) of
        MgrPid ->
            ets:give_away(TableId, CliPid, etsmgr),
            Clients2 = cli_pid_link(CliPid, Clients),
            {ok, MgrPid, TableId, #{tabid => TableId, clipid => CliPid}, Clients2};
        CliPid ->
            Clients2 = cli_pid_link(CliPid, Clients),
            {ok, MgrPid, TableId, #{tabid => TableId, clipid => CliPid}, Clients2};
        _OtherOwner ->
            {error, table_exists}
    end.

%%--------------------------------------------------------------------
%% @doc Link to pid and increment the client count for the pid.
%%
%% @end
%%--------------------------------------------------------------------
-spec cli_pid_link(pid(), map()) -> map().
cli_pid_link(CliPid, Clients) ->
    erlang:link(CliPid),
    case maps:find(CliPid, Clients) of
        error ->
            maps:put(CliPid, 1, Clients);
        {ok, Count} ->
            maps:update(CliPid, Count + 1, Clients)
    end.

%%--------------------------------------------------------------------
%% @doc handle the `add_table' request.
%%
%% Create a new table entry, however, if the `TableName' already
%% exists, we will perform further checks.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_add_table(atom(), ets:tid(), pid(), map(), map())
                      -> {ok, pid(), ets:tid(), map(), map()} | {error, term()}.
handle_add_table(TableName, TableId, CliPid, Tables, Clients) ->
    logger:info("~p:handle_add_table: TableName=~p, TableId=~p, CliPid=~p.",
                [?SERVER, TableName, TableId, CliPid]),
    ETStabid = ets:info(TableId, id),
    Result = case maps:find(TableName, Tables) of
        error -> %% i.e. no table entry
            check_table_ets_entry(ETStabid, CliPid, Clients);
        {ok, Table} ->
            check_table_entry(Table, ETStabid, CliPid, Clients)
    end,
    case Result of
        {ok, MgrPid, ETStabid, Table2, Clients2} ->
            {ok, MgrPid, ETStabid, maps:put(TableName, Table2, Tables), Clients2};
        Error = {error, _Reason} ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Check the status of an ETS table, and create/update table entry.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_table_ets_entry(ets:tid(), pid(), map())
                           ->  {ok, pid(), ets:tid(), map(), map()} | {error, term()}.
check_table_ets_entry(TableId, CliPid, Clients) ->
    MgrPid = self(),
    case ets:info(TableId, owner) of
        MgrPid ->
            new_table_entry(TableId, CliPid, Clients);
        CliPid ->
            new_table_entry(TableId, CliPid, Clients);
        _OtherOwner ->
            {error, not_owner}
    end.

%%--------------------------------------------------------------------
%% @doc Check, then create/update a table entry.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_table_entry(map(), ets:tid(), pid(), map())
                       -> {ok, pid(), ets:tid(), map()} | {error, term()}.
check_table_entry(Table, TableId, CliPid, Clients) ->
    {ok, TableTabid} = maps:find(tabid, Table),
    {ok, TableClipid} = maps:find(clipid, Table),
    case ets:info(TableTabid) of
        undefined -> %% we have a dead entry
            Clients2 = cli_pid_unlink(TableClipid, Clients),
            new_table_entry(TableId, CliPid, Clients2);
        _ when TableId =/= TableTabid ->
            {error, table_exists};
        _ ->
            MgrPid = self(),
            case ets:info(TableId, owner) of
                Pid when Pid == MgrPid orelse Pid == CliPid ->
                    Clients2 = cli_pid_unlink(TableClipid, Clients),
                    new_table_entry(TableId, CliPid, Clients2);
                _OtherOwner ->
                    {error, not_owner}
            end
    end.

%%--------------------------------------------------------------------
%% @doc Handle a del_table request.
%%
%% The table entry for `TableName' is removed, and the client is
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
%% </ol>
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_del_table(atom(), pid(), map(), map()) -> {ok, pid(), map(), map()} | {error, term()}.
handle_del_table(TableName, CliPid, Tables, Clients) ->
    logger:info("~p:handle_del_table: TableName=~p, CliPid=~p.",
                [?SERVER, TableName, CliPid]),
    case maps:find(TableName, Tables) of
        error ->
            {error, no_such_table_entry};
        {ok, Table} ->
            case del_table_check(CliPid, Table) of
                {ok, TableClipid} ->
                    Clients2 = cli_pid_unlink(TableClipid, Clients),
                    MgrPid = self(),
                    {ok, MgrPid, maps:remove(TableName, Tables), Clients2};
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
%% We will also remove the entry if the ETS table no longer exists.
%%
%% @end
%%--------------------------------------------------------------------
-spec del_table_check(pid(), map()) -> {ok, pid()} | {error, term()}.
del_table_check(CliPid, Table) ->
    TableTabid = maps:get(tabid, Table),
    TableClipid = maps:get(clipid, Table),
    case CliPid of
        TableClipid ->
            {ok, TableClipid};
        _ ->
            case ets:info(TableTabid, owner) of
                undefined ->
                    {ok, TableClipid};
                TableClipid ->
                    {ok, TableClipid};
                _OtherOwner ->
                    {error, not_owner}
            end
    end.

%%--------------------------------------------------------------------
%% @doc Conditionally unlink client pid.
%%
%% Check the count of tables for pid, and only unlink if this is the
%% last table of the client.
%%
%% @end
%%--------------------------------------------------------------------
-spec cli_pid_unlink(pid(), map()) -> map().
cli_pid_unlink(CliPid, Clients) ->
    case maps:find(CliPid, Clients) of
        error ->
            logger:warning("~p:cli_pid_unlink no entry for pid=~p - ignored.",
                           [?SERVER, CliPid]),
            Clients;
        {ok, 1} ->
            erlang:unlink(CliPid),
            maps:remove(CliPid, Clients);
        {ok, Count} ->
            maps:update(CliPid, Count - 1, Clients)
    end.

%%--------------------------------------------------------------------
%% @doc handle the `EXIT' message.
%%
%% We scan through all existing table entries and replace `CliPid' in
%% the matching entries with `none'.
%%
%% Note that the same client may appear multiple times!
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_exit(pid(), term(), map(), map()) -> {ok, map(), map()}.
handle_exit(CliPid, Reason, Tables, Clients) ->
    logger:notice("~p:handle_exit: from pid=~p, reason=~p.", [?SERVER, CliPid, Reason]),
    Tables2 = maps:map(
                fun (TableName, Table) ->
                        maps:map(
                          fun (clipid, TableClipid)
                                when TableClipid == CliPid ->
                                  logger:info("~p:handle_exit Client ~p for table ~p removed.",
                                              [?SERVER, CliPid, TableName]),
                                  none;
                              (_Key, Value) ->
                                  Value
                          end,
                          Table
                         )
                end,
                Tables
               ),
    Clients2 = maps:remove(CliPid, Clients),
    {ok, Tables2, Clients2}.

%%--------------------------------------------------------------------
%% @doc Handle the `ETS-TRANSFER' message.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_ets_transfer(ets:tid(), pid(), term(), map()) -> {ok, map()}.
handle_ets_transfer(TableId, CliPid, HeirData, Tables) ->
    logger:info("~p:handle_ets-transfer: for table=~p, from pid=~p, heir_data=~p.",
                   [?SERVER, TableId, CliPid, HeirData]),
    {ok, Tables}.

%%--------------------------------------------------------------------
