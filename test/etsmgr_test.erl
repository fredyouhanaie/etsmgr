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

%%-------------------------------------------------------------------
% helper function for the tests

%% check that the given instance of the server is a registered process
%%
check_server(Server, Inst_name) ->
    Process_name = etsmgr:inst_to_name(Server, Inst_name),
    erlang:whereis(Process_name) =/= undefined.

%% set up the tests
%%
setup() ->
    logger:set_primary_config(#{level => error}),
    etsmgr:start().

%% stop the server at end of test
%%
cleanup(_) ->
    etsmgr:stop().

%%-------------------------------------------------------------------

inst_name_test_() ->
    {"Instance name tests",
     [ {"instance name for unnamed server",
        ?_assertEqual(etsmgr_srv, etsmgr:inst_to_name(etsmgr_srv, etsmgr))},
       {"instance name for named server",
        ?_assertEqual(etsmgr_srv_aaa, etsmgr:inst_to_name(etsmgr_srv, aaa))}
     ]}.

%%-------------------------------------------------------------------

start_stop_test_() ->

    {setup,
     fun () -> logger:set_primary_config(#{level => error}) end,
     fun (_) -> ok end,
     [ {"start/stop the unnamed instance",
        [ {"start application",
           ?_assertEqual(ok, etsmgr:start())},
          {"start application again",
           ?_assertEqual({error, {already_started, etsmgr}}, etsmgr:start())},
          {"check application started",
           ?_assert(lists:keymember(etsmgr, 1, application:which_applications()))},
          {"check genserver is present",
           ?_assert(check_server(etsmgr_srv, etsmgr))},
          {"check supervisor is present",
           ?_assert(check_server(etsmgr_sup, etsmgr))},
          {"stop application",
           ?_assertEqual(ok, etsmgr:stop())},
          {"stop application again",
           ?_assertEqual({error, {not_started, etsmgr}}, etsmgr:stop())},
          {"check applicaton stopped",
           ?_assertNot(lists:keymember(etsmgr, 1, application:which_applications()))}
        ]},

       {"start/stop a named instance",
        [ {"start application",
           ?_assertEqual(ok, etsmgr:start(aaa))},
          {"start application again",
           ?_assertEqual({error, {already_started, aaa}}, etsmgr:start(aaa))},
          {"check application started",
           ?_assert(lists:keymember(aaa, 1, application:which_applications()))},
          {"check genserver is present",
           ?_assert(check_server(etsmgr_srv, aaa))},
          {"check supervisor is present",
           ?_assert(check_server(etsmgr_sup, aaa))},
          {"stop application",
           ?_assertEqual(ok, etsmgr:stop(aaa))},
          {"stop application again",
           ?_assertEqual({error, {not_started, aaa}}, etsmgr:stop(aaa))},
          {"check applicaton stopped",
           ?_assertNot(lists:keymember(aaa, 1, application:which_applications()))}
        ]}
     ]}.

%%-------------------------------------------------------------------

new_table() ->

    {ok, Mgr_pid, Table_id} = etsmgr:new_table(table1, ets_1, []),
    ?assert(is_pid(Mgr_pid)),
    ?assert(is_reference(Table_id)),
    ?assertEqual(self(), ets:info(Table_id, owner)),
    ?assertEqual(Mgr_pid, ets:info(Table_id, heir)),
    ?assertMatch({ok, Mgr_pid} when is_pid(Mgr_pid), etsmgr:del_table(table1)),
    ?assert(ets:delete(Table_id)),
    true.

new_table_test_() ->

    {"new table",
     [{setup, fun setup/0, fun cleanup/1,
       {inorder,
        [ {"add new table", ?_assert(new_table())}]
       }
      }
     ]}.

%%-------------------------------------------------------------------

add_table() ->
    ETS_id = ets:new(ets_1, []),
    {ok, Mgr_pid, Table_id} = etsmgr:add_table(table1, ETS_id),

    ?assert(self() == ets:info(Table_id, owner)),
    ?assert(Mgr_pid == ets:info(Table_id, heir)),
    ?assertEqual({ok, Mgr_pid}, etsmgr:del_table(table1)),
    ?assert(ets:delete(Table_id)),
    true.

add_table_test_() ->

    {"add table",
     [{setup, fun setup/0, fun cleanup/1,
       {inorder,
        [ {"add existing table", ?_assert(add_table())}]
       }
      }
     ]}.

%%-------------------------------------------------------------------

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

%%-------------------------------------------------------------------
