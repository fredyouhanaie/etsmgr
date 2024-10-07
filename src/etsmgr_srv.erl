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
%% If `Table_name' is not known to `etsmgr', a new ETS table will be
%% created, the calling process will be the owner of the table, with
%% `etsmgr' the heir, and `etsmgr' will be linked to the client
%% process.
%%
%% If an entry for `Table_name' already exists, then either the client
%% has restarted following a crash, or there is a name conflict with
%% another application. In the case of a restart, `etsmgr' will behave
%% as if this is a fresh start, but the table will not be created and
%% no data will be lost. In the case of a name conflict, an error will
%% be returned.
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
del_table(Inst_name, Table_name) ->
    Server_name = etsmgr:inst_to_name(?SERVER, Inst_name),
    gen_server:call(Server_name, {del_table, Table_name}).

%%--------------------------------------------------------------------
%% @doc Return the tables currently under management.
%%
%% This is primarily for debugging and troubleshooting.
%%
%% A map is returned where each entry has the client supplied
%% `Table_name' as key, and a further map as the value. The inner map
%% contains details of the corresponding ETS table being managed.
%%
%% @end
%%--------------------------------------------------------------------
-spec info(atom()) -> map().
info(Inst_name) ->
    Server_name = etsmgr:inst_to_name(?SERVER, Inst_name),
    gen_server:call(Server_name, {info}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom()) -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore.
start_link(Inst_name) ->
    Server_name = etsmgr:inst_to_name(?SERVER, Inst_name),
    gen_server:start_link({local, Server_name}, ?MODULE, Inst_name, []).

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
init(Inst_name) ->
    process_flag(trap_exit, true),
    {ok, #state{inst_name=Inst_name, tables=maps:new(), clients=maps:new()}}.

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

handle_call({new_table, Table_name, ETS_name, ETS_opts}, {Cli_pid, _Cli_ref}=_From, State) ->
    case handle_new_table(Table_name, ETS_name, ETS_opts, Cli_pid,
                          State#state.tables, State#state.clients) of
        {ok, Mgr_pid, Table_id, Tables, Clients} ->
            {reply, {ok, Mgr_pid, Table_id}, State#state{tables=Tables, clients=Clients}};
        Error = {error, _Reason} ->
            {reply, Error, State}
    end;

handle_call({add_table, Table_name, Table_id}, {Cli_pid, _Cli_ref}=_From, State) ->
    case handle_add_table(Table_name, Table_id, Cli_pid, State#state.tables, State#state.clients) of
        {ok, Mgr_pid, Table_id, Tables, Clients} ->
            {reply, {ok, Mgr_pid, Table_id},  State#state{tables=Tables, clients=Clients}};
        Error = {error, _Reason} ->
            {reply, Error, State}
    end;

handle_call({del_table, Table_name}, {Cli_pid, _Cli_ref}=_From, State) ->
    case handle_del_table(Table_name, Cli_pid, State#state.tables, State#state.clients) of
        {ok, Mgr_pid, Tables, Clients} ->
            {reply, {ok, Mgr_pid}, State#state{tables=Tables, clients=Clients}};
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
handle_info({'EXIT', Cli_pid, Reason}, State) ->
    Tables = State#state.tables,
    Clients = State#state.clients,
    {ok, Tables2, Clients2} = handle_exit(Cli_pid, Reason, Tables, Clients),
    {noreply, State#state{tables=Tables2, clients=Clients2}};

handle_info({'ETS-TRANSFER', Table_id, Cli_pid, Heir_data}, State) ->
    {ok, Tables2} = handle_ets_transfer(Table_id, Cli_pid, Heir_data, State#state.tables),
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
handle_new_table(Table_name, ETS_name, ETS_opts, Cli_pid, Tables, Clients) ->
    logger:info("~p:handle_new_table: Table_name=~p, ETS_name=~p, ETS_opts=~p, Cli_pid=~p.",
                [?SERVER, Table_name, ETS_name, ETS_opts, Cli_pid]),
    Result = case maps:find(Table_name, Tables) of
                 error -> %% error is good, we create new table and entry
                     new_table_ets(ETS_name, ETS_opts, Cli_pid, Clients);
                 {ok, Table} ->
                     {ok, Table_tid} = maps:find(tabid, Table),
                     case ets:info(Table_tid) of
                         undefined -> %% dead table entry, we create new table and entry
                             new_table_ets(ETS_name, ETS_opts, Cli_pid, Clients);
                         _ -> %% We have a live entry, check/update entry
                             new_table_entry(Table_tid, Cli_pid, Clients)
                     end
             end,

    case Result of
        {ok, Mgr_pid, Table_id, Table2, Clients2} ->
            {ok, Mgr_pid, Table_id, maps:put(Table_name, Table2, Tables), Clients2};
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
new_table_ets(ETS_name, ETS_opts, Cli_pid, Clients) ->
    try ets:new(ETS_name, ETS_opts) of
        ETS_table ->
            Table_id = ets:info(ETS_table, id),
            new_table_entry(Table_id, Cli_pid, Clients)
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
new_table_entry(Table_id, Cli_pid, Clients) ->
    Mgr_pid = self(),
    case ets:info(Table_id, owner) of
        Mgr_pid ->
            ets:give_away(Table_id, Cli_pid, etsmgr),
            Clients2 = cli_pid_link(Cli_pid, Clients),
            {ok, Mgr_pid, Table_id, #{tabid => Table_id, clipid => Cli_pid}, Clients2};
        Cli_pid ->
            Clients2 = cli_pid_link(Cli_pid, Clients),
            {ok, Mgr_pid, Table_id, #{tabid => Table_id, clipid => Cli_pid}, Clients2};
        _Other_owner ->
            {error, table_exists}
    end.

%%--------------------------------------------------------------------
%% @doc Link to pid and increment the client count for the pid.
%%
%% @end
%%--------------------------------------------------------------------
-spec cli_pid_link(pid(), map()) -> map().
cli_pid_link(Cli_pid, Clients) ->
    erlang:link(Cli_pid),
    case maps:find(Cli_pid, Clients) of
        error ->
            maps:put(Cli_pid, 1, Clients);
        {ok, Count} ->
            maps:update(Cli_pid, Count + 1, Clients)
    end.

%%--------------------------------------------------------------------
%% @doc handle the `add_table' request.
%%
%% Create a new table entry, however, if the `Table_name' already
%% exists, we will perform further checks.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_add_table(atom(), ets:tid(), pid(), map(), map())
                      -> {ok, pid(), ets:tid(), map(), map()} | {error, term()}.
handle_add_table(Table_name, Table_id, Cli_pid, Tables, Clients) ->
    logger:info("~p:handle_add_table: Table_name=~p, Table_id=~p, Cli_pid=~p.",
                [?SERVER, Table_name, Table_id, Cli_pid]),
    ETS_tabid = ets:info(Table_id, id),
    Result = case maps:find(Table_name, Tables) of
        error -> %% i.e. no table entry
            check_table_ets_entry(ETS_tabid, Cli_pid, Clients);
        {ok, Table} ->
            check_table_entry(Table, ETS_tabid, Cli_pid, Clients)
    end,
    case Result of
        {ok, Mgr_pid, ETS_tabid, Table2, Clients2} ->
            {ok, Mgr_pid, ETS_tabid, maps:put(Table_name, Table2, Tables), Clients2};
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
check_table_ets_entry(Table_id, Cli_pid, Clients) ->
    Mgr_pid = self(),
    case ets:info(Table_id, owner) of
        Mgr_pid ->
            new_table_entry(Table_id, Cli_pid, Clients);
        Cli_pid ->
            new_table_entry(Table_id, Cli_pid, Clients);
        _Other_owner ->
            {error, not_owner}
    end.

%%--------------------------------------------------------------------
%% @doc Check, then create/update a table entry.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_table_entry(map(), ets:tid(), pid(), map())
                       -> {ok, pid(), ets:tid(), map()} | {error, term()}.
check_table_entry(Table, Table_id, Cli_pid, Clients) ->
    {ok, Table_tabid} = maps:find(tabid, Table),
    {ok, Table_clipid} = maps:find(clipid, Table),
    case ets:info(Table_tabid) of
        undefined -> %% we have a dead entry
            Clients2 = cli_pid_unlink(Table_clipid, Clients),
            new_table_entry(Table_id, Cli_pid, Clients2);
        _ when Table_id =/= Table_tabid ->
            {error, table_exists};
        _ ->
            Mgr_pid = self(),
            case ets:info(Table_id, owner) of
                Pid when Pid == Mgr_pid orelse Pid == Cli_pid ->
                    Clients2 = cli_pid_unlink(Table_clipid, Clients),
                    new_table_entry(Table_id, Cli_pid, Clients2);
                _Other_owner ->
                    {error, not_owner}
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
%% </ol>
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_del_table(atom(), pid(), map(), map()) -> {ok, pid(), map(), map()} | {error, term()}.
handle_del_table(Table_name, Cli_pid, Tables, Clients) ->
    logger:info("~p:handle_del_table: Table_name=~p, Cli_pid=~p.",
                [?SERVER, Table_name, Cli_pid]),
    case maps:find(Table_name, Tables) of
        error ->
            {error, no_such_table_entry};
        {ok, Table} ->
            case del_table_check(Cli_pid, Table) of
                {ok, Table_clipid} ->
                    Clients2 = cli_pid_unlink(Table_clipid, Clients),
                    Mgr_pid = self(),
                    {ok, Mgr_pid, maps:remove(Table_name, Tables), Clients2};
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
del_table_check(Cli_pid, Table) ->
    Table_tabid = maps:get(tabid, Table),
    Table_clipid = maps:get(clipid, Table),
    case Cli_pid of
        Table_clipid ->
            {ok, Table_clipid};
        _ ->
            case ets:info(Table_tabid, owner) of
                undefined ->
                    {ok, Table_clipid};
                Table_clipid ->
                    {ok, Table_clipid};
                _Other_owner ->
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
cli_pid_unlink(Cli_pid, Clients) ->
    case maps:find(Cli_pid, Clients) of
        error ->
            logger:warning("~p:cli_pid_unlink no entry for pid=~p - ignored.",
                           [?SERVER, Cli_pid]),
            Clients;
        {ok, 1} ->
            erlang:unlink(Cli_pid),
            maps:remove(Cli_pid, Clients);
        {ok, Count} ->
            maps:update(Cli_pid, Count - 1, Clients)
    end.

%%--------------------------------------------------------------------
%% @doc handle the `EXIT' message.
%%
%% We scan through all existing table entries and replace `Cli_pid' in
%% the matching entries with `none'.
%%
%% Note that the same client may appear multiple times!
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_exit(pid(), term(), map(), map()) -> {ok, map(), map()}.
handle_exit(Cli_pid, Reason, Tables, Clients) ->
    logger:notice("~p:handle_exit: from pid=~p, reason=~p.", [?SERVER, Cli_pid, Reason]),
    Tables2 = maps:map(
                fun (Table_name, Table) ->
                        maps:map(
                          fun (clipid, Table_clipid)
                                when Table_clipid == Cli_pid ->
                                  logger:info("~p:handle_exit Client ~p for table ~p removed.",
                                              [?SERVER, Cli_pid, Table_name]),
                                  none;
                              (_Key, Value) ->
                                  Value
                          end,
                          Table
                         )
                end,
                Tables
               ),
    Clients2 = maps:remove(Cli_pid, Clients),
    {ok, Tables2, Clients2}.

%%--------------------------------------------------------------------
%% @doc Handle the `ETS-TRANSFER' message.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_ets_transfer(ets:tid(), pid(), term(), map()) -> {ok, map()}.
handle_ets_transfer(Table_id, Cli_pid, Heir_data, Tables) ->
    logger:info("~p:handle_ets-transfer: for table=~p, from pid=~p, heir_data=~p.",
                   [?SERVER, Table_id, Cli_pid, Heir_data]),
    {ok, Tables}.

%%--------------------------------------------------------------------
