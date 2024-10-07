% -*- indent-tabs-mode:nil; -*-
%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2019, Fred Youhanaie
%%% @doc
%%%
%%% Main `etsmgr' application.
%%%
%%% This module is for managing the standalone instances of `etsmgr'
%%% application.
%%%
%%% None of the functions in this module should be called
%%% directly. Instead, use the `start/0,1' and `stop/0,1' functions in
%%% the `etsmgr' module.
%%%
%%% @end
%%% Created :  1 Apr 2019 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(etsmgr_app).

-behaviour(application).

%% Application callbacks
-export([start/2, start_phase/3, stop/1, prep_stop/1,
         config_change/3]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%% @end
%%--------------------------------------------------------------------
-spec start(normal, atom()) -> {ok, pid()} | {error, term()}.
start(normal, InstName) ->
    case etsmgr_sup:start_link(InstName) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% top supervisor of the tree.
%% Starts an application with included applications, when
%% synchronization is needed between processes in the different
%% applications during startup.
%% @end
%%--------------------------------------------------------------------
-spec start_phase(Phase :: atom(),
                  StartType :: normal |
                               {takeover, Node :: node()} |
                               {failover, Node :: node()},
                  PhaseArgs :: term()) -> ok | {error, Reason :: term()}.
start_phase(_Phase, _StartType, _PhaseArgs) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec stop(State :: term()) -> any().
stop(_State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called when an application is about to be stopped,
%% before shutting down the processes of the application.
%% @end
%%--------------------------------------------------------------------
-spec prep_stop(State :: term()) -> NewState :: term().
prep_stop(State) ->
    State.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by an application after a code replacement,
%% if the configuration parameters have changed.
%% @end
%%--------------------------------------------------------------------
-spec config_change(Changed :: [{Par :: atom(), Val :: term()}],
                    New :: [{Par :: atom(), Val :: term()}],
                    Removed :: [Par :: atom()]) -> ok.
config_change(_Changed, _New, _Removed) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
