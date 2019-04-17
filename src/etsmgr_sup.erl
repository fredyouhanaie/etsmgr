%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2019, Fred Youhanaie
%%% @doc
%%%
%%% Supervisor for the `etsmgr' application.
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
