%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2024, Fred Youhanaie
%%% @doc
%%%
%%% Client module for the `etscounter' application.
%%%
%%% Provides helper functions for interacting with the server.
%%%
%%% @end
%%% Created : 25 Nov 2024 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(etscounter).

-export([start/0, stop/0]).
-export([count/0, check/0, die/0]).

%%--------------------------------------------------------------------
%%
start() ->
    application:ensure_started(etsmgr),
    application:start(etscounter).

%%--------------------------------------------------------------------
%%
stop() ->
    application:stop(etscounter).

%%--------------------------------------------------------------------
%%
count() ->
    etscounter_srv:count().

%%--------------------------------------------------------------------
%%
check() ->
    etscounter_srv:check().

%%--------------------------------------------------------------------
%%
die() ->
    etscounter_srv:die().

%%--------------------------------------------------------------------
