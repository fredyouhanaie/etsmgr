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

check_server(Server, Inst_name) ->
    Process_name = etsmgr:inst_to_name(Server, Inst_name),
    erlang:whereis(Process_name) =/= undefined.

etsmgr_inst_name_test() ->
    ?assert(etsmgr_srv == etsmgr:inst_to_name(etsmgr_srv, etsmgr)),
    ?assert(etsmgr_srv_aaa == etsmgr:inst_to_name(etsmgr_srv, aaa)).

etsmgr_start_stop_1_test() ->
    ok = etsmgr:start(),
    ?assert(lists:keymember(etsmgr, 1, application:which_applications())),
    ?assert(check_server(etsmgr_srv, etsmgr)),
    ?assert(check_server(etsmgr_sup, etsmgr)),
    ok = etsmgr:stop(),
    ?assertNot(lists:keymember(etsmgr, 1, application:which_applications())).

etsmgr_start_stop_2_test() ->
    Name = aaa,
    ok = etsmgr:start(Name),
    ?assert(lists:keymember(Name, 1, application:which_applications())),
    ?assert(check_server(etsmgr_srv, Name)),
    ?assert(check_server(etsmgr_sup, Name)),
    ok = etsmgr:stop(Name),
    ?assertNot(lists:keymember(Name, 1, application:which_applications())).

etsmgr_new_table_1_test() ->
    ok = application:ensure_started(etsmgr),
    {ok, Mgr_pid, Table_id} = etsmgr:new_table(table1, ets_1, []),
    ?assert(self() == ets:info(Table_id, owner)),
    ?assert(Mgr_pid == ets:info(Table_id, heir)),
    {ok, Mgr_pid} = etsmgr:del_table(table1),
    ok = etsmgr:stop().

etsmgr_add_table_1_test() ->
    ok = application:ensure_started(etsmgr),
    ETS_id = ets:new(ets_1, []),
    {ok, Mgr_pid, Table_id} = etsmgr:add_table(table1, ETS_id),
    ?assert(self() == ets:info(Table_id, owner)),
    ?assert(Mgr_pid == ets:info(Table_id, heir)),
    {ok, Mgr_pid} = etsmgr:del_table(table1),
    ok = etsmgr:stop().

etsmgr_del_table_1_test() ->
    ok = application:ensure_started(etsmgr),
    ETS_id = ets:new(ets_1, []),
    {ok, Mgr_pid, Table_id} = etsmgr:add_table(table1, ETS_id),
    ?assert(self() == ets:info(Table_id, owner)),
    ?assert(Mgr_pid == ets:info(Table_id, heir)),
    {ok, Mgr_pid} = etsmgr:del_table(table1),
    ok = etsmgr:stop().

etsmgr_del_table_2_test() ->
    ok = application:ensure_started(etsmgr),
    {error, no_such_table_entry} = etsmgr:del_table(table1),
    ok = etsmgr:stop().
