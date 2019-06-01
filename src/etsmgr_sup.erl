% -*- indent-tabs-mode:nil; -*-
%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2019, Fred Youhanaie
%%% @doc
%%%
%%% This is the supervisor for the `etsmgr' server. It manages a
%%% single instance of the `etsmgr_srv' server.
%%%
%%% For an standalone instance of `etsmgr', the supervisor will be
%%% ultimately started via the `etsmgr:start/0,1' functions.
%%%
%%% For an embedded instance of `etsmgr', and if this extra supervisor
%%% is required, the supervisor should be added as a child of the
%%% client's supervisor. An instance name will need to be given to the
%%% `etsmgr_sup:start_link/1' function. Below is an example snippet of
%%% a typical `supervisor:child_spec()' entry:
%%%
%%% <pre>
%%%   #{id => 'etsmgr_sup',
%%%     start => {'etsmgr_sup', start_link, [Inst_name]},
%%%     type => supervisor}
%%% </pre>
%%%
%%% @end
%%% Created :  1 Apr 2019 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(etsmgr_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom()) -> {ok, Pid :: pid()} |
                      {error, {already_started, Pid :: pid()}} |
                      {error, {shutdown, term()}} |
                      {error, term()} |
                      ignore.
start_link(Inst_name) ->
    Server_name = etsmgr:inst_to_name(?SERVER, Inst_name),
    supervisor:start_link({local, Server_name}, ?MODULE, Inst_name).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(atom()) ->
                  {ok, {SupFlags :: supervisor:sup_flags(),
                        [ChildSpec :: supervisor:child_spec()]}} |
                  ignore.
init(Inst_name) ->

    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    Child1 = #{id => 'etsmgr_srv',
               start => {'etsmgr_srv', start_link, [Inst_name]},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => ['etsmgr_srv']},

    {ok, {SupFlags, [Child1]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
