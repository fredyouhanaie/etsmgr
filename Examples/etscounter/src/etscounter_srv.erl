%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2019, Fred Youhanaie
%%% @doc etscounter main server.
%%%
%%% @end
%%% Created :  3 Jun 2019 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(etscounter_srv).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0]).
-export([count/0, check/0, die/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {etsmgr_pid, table_id}).

%%%===================================================================
%%% API
%%%===================================================================

count() ->
    gen_server:cast(?SERVER, {count}).

check() ->
    gen_server:cast(?SERVER, {check}).

die() ->
    gen_server:cast(?SERVER, {die}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
		      {error, Error :: {already_started, pid()}} |
		      {error, Error :: term()} |
		      ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
			      {ok, State :: term(), Timeout :: timeout()} |
			      {ok, State :: term(), hibernate} |
			      {stop, Reason :: term()} |
			      ignore.
init([]) ->
    process_flag(trap_exit, true),
    etsmgr:wait4etsmgr(),
    {ok, Etsmgr_pid, Table_id} = etsmgr:new_table(etscounter, etscounter, []),
    %% initilize counter, if table is new
    case ets:lookup(Table_id, count) of
        [] ->
            ets:insert(Table_id, {count, 0});
        _ ->
            pass
    end,
    {ok, #state{etsmgr_pid=Etsmgr_pid, table_id=Table_id}}.

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
handle_call(Request, From, State) ->
    logger:warning("~p: unexpected call ~p from ~p - ignored", [?SERVER, Request, From]),
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
handle_cast({count}, State) ->
    Table_id = State#state.table_id,
    Result = ets:update_counter(Table_id, count, 1),
    logger:notice("~p: Counter ~p.", [?SERVER, Result]),
    {noreply, State};

handle_cast({check}, State) ->
    Table_id = State#state.table_id,
    Result = ets:tab2list(Table_id),
    logger:notice("~p: Table: ~p, Data ~p.", [?SERVER, Table_id, Result]),
    {noreply, State};

handle_cast({die}, State) ->
    exit(kill),
    Table_id = State#state.table_id,
    Result = ets:tab2list(Table_id),
    logger:notice("~p: Table: ~p, Data ~p.", [?SERVER, Table_id, Result]),
    {noreply, State};

handle_cast(Request, State) ->
    logger:warning("~p: unexpected cast ~p - ignored", [?SERVER, Request]),
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

handle_info({'ETS-TRANSFER', Table_id, From_pid, Gift_data}, State) ->
    logger:notice("~p: got a table ~p from ~p, with data ~p.", [?SERVER, Table_id, From_pid, Gift_data]),
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, State) ->
    Etsmgr_pid = State#state.etsmgr_pid,
    case Pid of
        Etsmgr_pid ->
            logger:warning("~p: etsmgr (~p) has died, with Reason ~p.", [?SERVER, Pid, Reason]),
            logger:notice("~p: waiting for etsmgr to restart.", [?SERVER]),
            etsmgr:wait4etsmgr(),
            {ok, Etsmgr_pid2, _Table_id} = etsmgr:add_table(etscounter, State#state.table_id),
            State2 = State#state{etsmgr_pid=Etsmgr_pid2},
            {noreply, State2};
        _ ->
            logger:warning("~p: unexpected EXIT from unknown process ~p, with Reason ~p - ignored.",
                           [?SERVER, Pid, Reason]),
            {noreply, State}
    end;

handle_info(Info, State) ->
    logger:warning("~p: unexpected message ~p - ignored", [?SERVER, Info]),
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
terminate(_Reason, State) ->
    etsmgr:del_table(etscounter),
    ets:delete(State#state.table_id),
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
