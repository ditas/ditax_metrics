%%%-------------------------------------------------------------------
%%% @author pravosudov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Oct 2017 13:25
%%%-------------------------------------------------------------------
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

-spec(add(Metric :: #metric{} | atom()) -> ok | {error, table_already_exists}).

add(#metric{name = MetricName} = Metric) ->
    case ets:info(MetricName) of
        undefined ->
            gen_server:call(?SERVER, {add, Metric});
        _ ->
            lager:error("~p table already exists", [MetricName]),
            {error, table_already_exists}
    end;
add(MetricName) ->
    case ets:info(MetricName) of
        undefined ->
            gen_server:call(?SERVER, {add, MetricName});
        _ ->
            lager:error("~p table already exists", [MetricName]),
            {error, table_already_exists}
    end.

delete() ->
    gen_server:cast(?SERVER, delete).

delete(MetricName) ->
    case ets:info(MetricName) of
        undefined -> lager:error("~p table does not exist", [MetricName]);
        _ -> gen_server:cast(?SERVER, {delete, MetricName})
    end.

inc(MetricName) ->
    case ets:info(MetricName) of
        undefined -> lager:error("~p table does not exist", [MetricName]);
        _ -> gen_server:cast(?SERVER, {MetricName, increment, 1})
    end.

inc(MetricName, Value) ->
    case ets:info(MetricName) of
        undefined -> lager:error("~p table does not exist", [MetricName]);
        _ -> gen_server:cast(?SERVER, {MetricName, increment, Value})
    end.

-spec(get(MetricName :: atom()) -> Res :: integer() | {error, table_does_not_exist}).

get(MetricName) ->
    case ets:info(MetricName) of
        undefined ->
            lager:error("~p table does not exist", [MetricName]),
            {error, table_does_not_exist};
        _ -> gen_server:call(?SERVER, {get, MetricName})
    end.

-spec(get(MetricName :: atom(), Period :: integer()) -> Res :: integer() | {error, table_does_not_exist}).

get(MetricName, Period) ->
    case ets:info(MetricName) of
        undefined ->
            lager:error("~p table does not exist", [MetricName]),
            {error, table_does_not_exist};
        _ -> gen_server:call(?SERVER, {get, MetricName, Period})
    end.

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
    State = lists:foldl(fun(El, S) ->
        create_metric(El, S)
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
handle_call({add, Metric}, _From, State) ->
    State1 = create_metric(Metric, State),
    {reply, ok, State1};
handle_call({get, MetricName}, _From, State) ->
    case lists:keyfind(MetricName, #created_metric.name, State#state.metrics) of
        #created_metric{period = Period} when Period =/= undefined ->
            StartTime = erlang:system_time(second) - Period,
            MS = [{#metric_data{time = '$1',value = '$2'},
                   [{'>=','$1',StartTime}],
                   ['$_']}],
            Selection = ets:select(MetricName, MS),
            Count = lists:foldl(fun(#metric_data{value = V}, Acc) -> Acc + V end, 0, Selection),
            Count;
        #created_metric{} ->
            Count = ets:foldl(fun(#metric_data{value = V}, Acc) -> Acc + V end, 0, MetricName),
            Count
    end,
    {reply, Count, State};
handle_call({get, MetricName, Period}, _From, State) ->
    StartTime = erlang:system_time(second) - Period,
    MS = [{#metric_data{time = '$1',value = '$2'},
           [{'>=','$1',StartTime}],
           ['$_']}],
    Selection = ets:select(MetricName, MS),
    Count = lists:foldl(fun(#metric_data{value = V}, Acc) -> Acc + V end, 0, Selection),
    {reply, Count, State};
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
handle_cast({MetricName, increment, Value}, State) ->
    Key = erlang:system_time(second),
    case ets:lookup(MetricName, Key) of
        [] ->
            ets:insert(MetricName, #metric_data{time = Key, value = Value});
        _ ->
            _Result = ets:update_counter(MetricName, erlang:system_time(second), {#metric_data.value, Value})
    end,
    {noreply, State};
handle_cast(delete, State) ->
    State1 = lists:foldl(fun(M, S) ->
        ets:delete(element(1, M)),
        CurrentMetrics = S#state.metrics,
        S#state{metrics = lists:delete(M, CurrentMetrics)}
    end, State, State#state.metrics),
    {noreply, State1};
handle_cast({delete, MetricName}, State) ->
    ets:delete(MetricName),
    CurrentMetrics = State#state.metrics,
    State1 = State#state{metrics = lists:keydelete(MetricName, 1, CurrentMetrics)},
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
handle_info({reset, MetricName, Period}, State) ->
    StartTime = erlang:system_time(second) - Period,
    MS = [{{'_', '$1','$2'},
           [{'<','$1',StartTime}],
           [true]}],

    %% TODO: проверить производительность
    _Result = ets:select_delete(MetricName, MS),

    {noreply, State};
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

create_metric(Metric, State) ->
    CurrentMetrics = State#state.metrics,
    case is_record(Metric, metric) of
        true ->
            ets:new(Metric#metric.name, [ordered_set, named_table, {keypos, #metric_data.time}]),
            case Metric#metric.period of
                undefined ->
                    State#state{metrics = [#created_metric{name = Metric#metric.name} | CurrentMetrics]};
                Period ->
                    {ok, TRef} = timer:send_interval(Period * 1000, {reset, Metric#metric.name, Period}),
                    State#state{metrics = [#created_metric{name = Metric#metric.name, period = Period, timer_ref = TRef} | CurrentMetrics]}
            end;
        false ->
            ets:new(Metric, [ordered_set, named_table, {keypos, #metric_data.time}]),
            State#state{metrics = [#created_metric{name = Metric} | CurrentMetrics]}
    end.