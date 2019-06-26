%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2019, Fred Youhanaie
%%% @doc
%%%
%%% Eunit tests for the etsmgr application.
%%%
%%% @end
%%% Created :  2 Apr 2019 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------

-module(etsmgr_test).

-include_lib("eunit/include/eunit.hrl").

% helper function for the tests
check_server(Server, Inst_name) ->
    Process_name = etsmgr:inst_to_name(Server, Inst_name),
    erlang:whereis(Process_name) =/= undefined.

% test the utility function
etsmgr_inst_name_test() ->
    ?assert(etsmgr_srv == etsmgr:inst_to_name(etsmgr_srv, etsmgr)),
    ?assert(etsmgr_srv_aaa == etsmgr:inst_to_name(etsmgr_srv, aaa)).

% start/stop the unnamed instance
etsmgr_start_stop_1_test() ->

    ok = etsmgr:start(),
    ?assert(lists:keymember(etsmgr, 1, application:which_applications())),
    ?assert(check_server(etsmgr_srv, etsmgr)),
    ?assert(check_server(etsmgr_sup, etsmgr)),

    ok = etsmgr:stop(),
    ?assertNot(lists:keymember(etsmgr, 1, application:which_applications())).

% start/stop a named instance
etsmgr_start_stop_2_test() ->
    Name = aaa,

    ok = etsmgr:start(Name),
    ?assert(lists:keymember(Name, 1, application:which_applications())),
    ?assert(check_server(etsmgr_srv, Name)),
    ?assert(check_server(etsmgr_sup, Name)),

    ok = etsmgr:stop(Name),
    ?assertNot(lists:keymember(Name, 1, application:which_applications())).

% simple new_table test
etsmgr_new_table_1_test() ->
    ok = application:ensure_started(etsmgr),

    {ok, Mgr_pid, Table_id} = etsmgr:new_table(table1, ets_1, []),
    ?assert(self() == ets:info(Table_id, owner)),
    ?assert(Mgr_pid == ets:info(Table_id, heir)),

    {ok, Mgr_pid} = etsmgr:del_table(table1),
    ets:delete(Table_id),

    ok = etsmgr:stop().

% simple add_table test
etsmgr_add_table_1_test() ->
    ok = application:ensure_started(etsmgr),

    ETS_id = ets:new(ets_1, []),
    {ok, Mgr_pid, Table_id} = etsmgr:add_table(table1, ETS_id),
    ?assert(self() == ets:info(Table_id, owner)),
    ?assert(Mgr_pid == ets:info(Table_id, heir)),

    {ok, Mgr_pid} = etsmgr:del_table(table1),
    ets:delete(Table_id),

    ok = etsmgr:stop().

% simple create/delete test
etsmgr_del_table_1_test() ->
    ok = application:ensure_started(etsmgr),

    ETS_id = ets:new(ets_1, []),
    {ok, Mgr_pid, Table_id} = etsmgr:add_table(table1, ETS_id),
    ?assert(self() == ets:info(Table_id, owner)),
    ?assert(Mgr_pid == ets:info(Table_id, heir)),

    {ok, Mgr_pid} = etsmgr:del_table(table1),
    ets:delete(Table_id),

    ok = etsmgr:stop().

% deleting a non-existant table should return error
etsmgr_del_table_2_test() ->
    ok = application:ensure_started(etsmgr),

    {error, no_such_table_entry} = etsmgr:del_table(table1),

    ok = etsmgr:stop().

% when a client has two, or more, tables under management, and we
% delete one, we should still be linked to the table manager.
etsmgr_del_table_3_test() ->
    ok = application:ensure_started(etsmgr),

    % create/manage the first table
    {ok, Mgr_pid, Table_id1} = etsmgr:new_table(table1, ets_1, []),
    ?assert(self() == ets:info(Table_id1, owner)),
    ?assert(Mgr_pid == ets:info(Table_id1, heir)),
    {links, Linked_pids1} = erlang:process_info(self(), links),
    ?assert(lists:member(Mgr_pid, Linked_pids1)),

    % create/manage the second table
    {ok, Mgr_pid, Table_id2} = etsmgr:new_table(table2, ets_2, []),
    ?assert(self() == ets:info(Table_id2, owner)),
    ?assert(Mgr_pid == ets:info(Table_id2, heir)),
    {links, Linked_pids2} = erlang:process_info(self(), links),
    ?assert(lists:member(Mgr_pid, Linked_pids2)),

    % delete/unmanage the first table
    {ok, Mgr_pid} = etsmgr:del_table(table1),
    ets:delete(Table_id1),

    % we should still be linked to the manager pid
    {links, Linked_pids3} = erlang:process_info(self(), links),
    ?assert(lists:member(Mgr_pid, Linked_pids3)),

    {ok, Mgr_pid} = etsmgr:del_table(table2),
    ets:delete(Table_id2),

    ok = etsmgr:stop().
