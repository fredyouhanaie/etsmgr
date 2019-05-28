%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2019, Fred Youhanaie
%%% @doc
%%%
%%% Client functions for `etsmgr'.
%%%
%%% @end
%%% Created :  1 Apr 2019 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------

-module(etsmgr).

%% API exports
-export([start/0, stop/0]).
-export([start/1, stop/1]).
-export([inst_to_name/2]).
-export([new_table/3, add_table/2, del_table/1, info/0]).
-export([new_table/4, add_table/3, del_table/2, info/1]).
-export([wait4etsmgr/0, wait4etsmgr/1]).

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Start the unnamed instance of the application.
%%
%% The unnamed instance is convenient as it can serve any application
%% within the same node.
%%
%% Internally the unnamed instance is in fact an instance with the
%% name `etsmgr'.
%%
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok | {error, term()}.
start() ->
    start(etsmgr).


%%--------------------------------------------------------------------
%% @doc Start a named instance of the application.
%%
%% The instance name should be an atom. The name `etsmgr' is reserved
%% for the "unnamed" instance. If the instance name is already in use,
%% then the call will fail.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(atom()) -> ok | {error, term()}.
start(etsmgr) ->
    application:start(etsmgr);

start(Inst_name) ->
    %% retrieve the main app resource file
    {ok, [{application, etsmgr, App_0}]} = file:consult(code:where_is_file("etsmgr.app")),

    %% add the instance name suffix to the list of application servers
    {registered, Server_names_0} = lists:keyfind(registered, 1, App_0),
    Server_names_1 = lists:map(
		       fun (S) -> inst_to_name(S, Inst_name) end,
		       Server_names_0),
    App_1 = lists:keyreplace(registered, 1, App_0, {registered, Server_names_1}),

    %% pass the instance name to the application as arg
    App_2 = lists:keyreplace(mod, 1, App_1, {mod, {etsmgr_app, Inst_name}}),

    application:load({application, Inst_name, App_2}),
    application:start(Inst_name).


%%--------------------------------------------------------------------
%% @doc Stop the unnamed instance of the application.
%%
%% The unnamed instance, or the instance known `etsmgr', will be
%% stopped. This is same as calling `stop(etsmgr)'.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok | {error, term()}.
stop() ->
    stop(etsmgr).

%%--------------------------------------------------------------------
%% @doc Stop a named instance of the application.
%%
%% The named instance will be stopped. Any ETS table, and data within,
%% where this instance is the owner, e.g. perhaps because the the main
%% application has died, will be lost.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(atom()) -> ok | {error, term()}.
stop(Inst_name) ->
    application:stop(Inst_name).

%%--------------------------------------------------------------------
%% @doc Create and manage an ETS table.
%%
%% The request is sent to the unnamed (aka `etsmgr') instance.
%%
%% See `new_table/4' for details.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_table(atom(), atom(), list()) -> {ok, pid(), ets:tid()} | {error, term()}.
new_table(Table_name, ETS_name, ETS_opts) ->
    new_table(etsmgr, Table_name, ETS_name, ETS_opts).


%%--------------------------------------------------------------------
%% @doc Create and manage an ETS table.
%%
%% This function is called by client software on start up, or on
%% crash/restart. `etsmgr' will check and create the table.
%%
%% If `Table_name' is not known to `etsmgr', a new ETS table will be
%% created, the calling process will be the owner of the table, with
%% `etsmgr' the heir, and `etsmgr' will be linked to the client
%% process.
%%
%% If `Table_name' is already known to the server, then either the
%% client has restarted following a crash, or there is a name conflict
%% with another application. In the case of a restart, `etsmgr' will
%% behave as if this is a fresh start, but the table will not be
%% created and no data will be lost. In the case of a name conflict,
%% an error will be returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_table(atom(), atom(), atom(), list()) -> {ok, pid(), ets:tid()} | {error, term()}.
new_table(Inst_name, Table_name, ETS_name, ETS_opts) ->
    etsmgr_srv:new_table(Inst_name, Table_name, ETS_name, ETS_opts).


%%--------------------------------------------------------------------
%% @doc Start managing an existing ETS table.
%%
%% This request is sent to the unnamed (aka `etsmgr') instance.
%%
%% See `add_table/3' for details.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_table(atom(), ets:tid()) -> {ok, pid(), ets:tid()} | {error, term()}.
add_table(Table_name, Table_id) ->
    add_table(etsmgr, Table_name, Table_id).


%%--------------------------------------------------------------------
%% @doc Start managing an existing ETS table.
%%
%% This function is typically called when an application needs
%% `etsmgr' to manage its ETS table(s), which already exist, either
%% because the application prefers to create its own tables, or it has
%% detected tat the instance of `etsmgr' that was already managing the
%% ETS tables has crashed and restarted.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_table(atom(), atom(), ets:tid()) -> {ok, pid(), ets:tid()} | {error, term()}.
add_table(Inst_name, Table_name, Table_id) ->
    etsmgr_srv:add_table(Inst_name, Table_name, Table_id).


%%--------------------------------------------------------------------
%% @doc Remove a table from the list of managed tables.
%%
%% This request is sent to the unnamed (aka `etsmgr') instance.
%%
%% See `del_table/2' for details.
%%
%% @end
%%--------------------------------------------------------------------
-spec del_table(atom()) -> ok | {error, term()}.
del_table(Table_name) ->
    del_table(etsmgr, Table_name).


%%--------------------------------------------------------------------
%% @doc Remove a table from the list of managed tables.
%%
%% This function is called by a client when it no longer needs the ETS
%% table to be managed by `etsmgr'. This is likely to be when the
%% client is shutting down.
%%
%% @end
%%--------------------------------------------------------------------
-spec del_table(atom(), atom()) -> ok | {error, term}.
del_table(Inst_name, Table_name) ->
    etsmgr_srv:del_table(Inst_name, Table_name).


%%--------------------------------------------------------------------
%% @doc Return the tables currently under management.
%%
%% The request is sent to the unnamed (aka `etsmgr') instance.
%%
%% See `info/1' for details.
%%
%% @end
%%--------------------------------------------------------------------
-spec info() -> map().
info() ->
    info(etsmgr).


%%--------------------------------------------------------------------
%% @doc Return the tables currently under management.
%%
%% A map of the table under management will be returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec info(atom()) -> map().
info(Inst_name) ->
    etsmgr_srv:info(Inst_name).


%%--------------------------------------------------------------------
%% @doc Block until the unnamed instance of the table manager has started.
%%
%% The function applies to the unname (aka `etsmgr') instance.
%%
%% See `wait4etsmgr/1' for details.
%%
%% @end
%%--------------------------------------------------------------------
-spec wait4etsmgr() -> ok.
wait4etsmgr() ->
    wait4etsmgr(etsmgr).


%%--------------------------------------------------------------------
%% @doc Block until a named instance of the table manager has started.
%%
%% This function is typically called when a client process detects
%% that an instance of `etsmgr' has died.
%%
%% This is a very simplistic function that will block, perhaps
%% forever, until the unnamed instance of `etsmgr' is up and running.
%%
%% Any sphisticated monitoring and restart scheme is beyond the scope
%% of this module. Such functionality deserves its own
%% module/application.
%%
%% @end
%%--------------------------------------------------------------------
-spec wait4etsmgr(atom()) -> ok.
wait4etsmgr(Inst_name) ->
    Server_name = inst_to_name(etsmgr_srv, Inst_name),
    wait4server(Server_name).


%%--------------------------------------------------------------------
%% @doc
%% Convert an instance name to longer prefixed name.
%%
%% This is used for obtaining the instance specific server/table
%% names. For example `inst_to_name(etsmgr_srv, aaa)' will return
%% `etsmgr_srv_aaa'.
%%
%% If the instance name is `etsmgr', then the prefix is returned
%% without an instance name suffix. For example
%% `inst_to_name(etsmgr_srv, etsmgr)' will return `etsmgr_srv'.
%%
%% @end
%%--------------------------------------------------------------------
-spec inst_to_name(atom(), atom()) -> atom().
inst_to_name(Prefix, etsmgr) ->
    Prefix;
inst_to_name(Prefix, Inst_name) ->
    list_to_atom(atom_to_list(Prefix) ++ "_" ++ atom_to_list(Inst_name)).


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Block until a registered process has started.
%%
%% We check for the registered process at one second intervals.
%%
%% @end
%%--------------------------------------------------------------------
-spec wait4server(atom()) -> ok.
wait4server(Server_name) ->
    case erlang:whereis(Server_name) of
	undefined ->
	    timer:sleep(1000),
	    wait4server(Server_name);
	Pid when is_pid(Pid) ->
	    ok
    end.
