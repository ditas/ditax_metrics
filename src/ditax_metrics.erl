-module(ditax_metrics).
-author("pravosudov").
-vsn("0.1.3").

-include_lib("../include/ditax_metrics.hrl").

-behaviour(gen_server).

%% API
-export([
    start_link/1,
    add/1,
    delete/0,
    delete/1,
    inc/1,
    inc/2,
    get/1,
    get/2
]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    metrics = []
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(MetricsList) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [MetricsList], []).

add(Metric) ->
    gen_server:cast(?SERVER, {add, Metric}).

delete() ->
    gen_server:cast(?SERVER, delete).

delete(MetricName) ->
    gen_server:cast(?SERVER, MetricName).

inc(MetricName) ->
    gen_server:cast(?SERVER, {MetricName, increment, 1}).

inc(MetricName, Value) ->
    gen_server:cast(?SERVER, {MetricName, increment, Value}).

get(MetricName) ->
    gen_server:call(?SERVER, {get, MetricName}).

get(MetricName, Period) ->
    gen_server:call(?SERVER, {get, MetricName, Period}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([MetricsList]) ->
    
    io:format("--------INIT ~p~n", [?MODULE]),
    
    State = lists:foldl(fun(M, S) ->
        case lists:member(M, S#state.metrics) of
            true -> S;
            false -> create_metric(M, S)
        end
    end, #state{}, MetricsList),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #state{}) ->
                     {reply, Reply :: term(), NewState :: #state{}} |
                     {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
                     {noreply, NewState :: #state{}} |
                     {noreply, NewState :: #state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
                     {stop, Reason :: term(), NewState :: #state{}}).
handle_call({get, MetricName}, _From, State) ->
    Res = case lists:member(MetricName, State#state.metrics) of
        true ->
            gen_server:call(MetricName, get);
        _ ->
            lager:error("~p table does not exist ~p", [MetricName, State#state.metrics]),
            {error, wrong_metric}
    end,
    {reply, Res, State};
handle_call({get, MetricName, Period}, _From, State) ->
    Res = case lists:member(MetricName, State#state.metrics) of
              true ->
                  gen_server:call(MetricName, {get, Period});
              _ ->
                  lager:error("~p table does not exist", [MetricName]),
                  {error, wrong_metric}
          end,
    {reply, Res, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({add, #metric{name = MetricName} = Metric}, State) ->
    State1 = case lists:member(MetricName, State#state.metrics) of
                 false ->
                     create_metric(Metric, State);
                 _ ->
                     lager:error("~p table already exists", [MetricName]),
                     State
             end,
    {noreply, State1};
handle_cast({add, MetricName}, State) ->
    State1 = case lists:member(MetricName, State#state.metrics) of
                false ->
                    create_metric(MetricName, State);
                _ ->
                    lager:error("~p table already exists", [MetricName]),
                    State
            end,
    {noreply, State1};
handle_cast({MetricName, increment, Value}, State) ->
    case lists:member(MetricName, State#state.metrics) of
        true ->
            gen_server:cast(MetricName, {increment, Value});
        _ ->
            lager:error("~p table does not exist", [MetricName])
    end,
    {noreply, State};
handle_cast(delete, State) ->
    State1 = lists:foldl(fun(M, S) ->
        ok = gen_server:cast(M, {delete, M}),
        CurrentMetrics = S#state.metrics,
        S#state{metrics = lists:delete(M, CurrentMetrics)}
    end, State, State#state.metrics),
    {noreply, State1};
handle_cast({delete, MetricName}, State) ->
    State1 = case lists:member(MetricName, State#state.metrics) of
        true ->
            ok = gen_server:cast(MetricName, {delete, MetricName}),
            CurrentMetrics = State#state.metrics,
            State#state{metrics = lists:keydelete(MetricName, 1, CurrentMetrics)};
        _ ->
            lager:error("~p table does not exist", [MetricName]),
            State
    end,
    {noreply, State1};
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
                     {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_metric(#metric{name = MetricName} = Metric, State) ->
    CurrentMetrics = State#state.metrics,
    {ok, _Pid} = ditax_metric:start_link(Metric),
    State#state{metrics = [MetricName | CurrentMetrics]};
create_metric(MetricName, State) ->
    CurrentMetrics = State#state.metrics,
    {ok, _Pid} = ditax_metric:start_link(MetricName),
    State#state{metrics = [MetricName | CurrentMetrics]}.