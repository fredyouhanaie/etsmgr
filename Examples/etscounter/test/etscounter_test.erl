%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2019, Fred Youhanaie
%%% @doc
%%%
%%% Eunit tests for the etsmgr application.
%%%
%%% @end
%%% Created :  2 Jun 2019 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------

-module(etscounter_test).

-include_lib("eunit/include/eunit.hrl").

-define(Delay, 1000).

dz_1_test() ->
    code:add_path("../../_build/default/lib/etsmgr/ebin/"),

    application:ensure_all_started(etsmgr),
    application:ensure_all_started(etscounter),

    etscounter_srv:count(),
    etscounter_srv:count(),

    timer:sleep(?Delay),
    erlang:exit(whereis(etscounter_srv), kill),

    etscounter_srv:count(),
    etscounter_srv:count(),

    timer:sleep(?Delay),
    etscounter_srv:die(),

    etscounter_srv:count(),
    etscounter_srv:count(),

    timer:sleep(?Delay),
    erlang:exit(whereis(etscounter_srv), kill),

    etscounter_srv:count(),
    etscounter_srv:count(),

    timer:sleep(?Delay),

    ok.
