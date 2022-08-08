-module(ditax_metric).
-author("ditas").

-include_lib("../include/ditax_metrics.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    name,
    period,
    timer_ref
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Metric) when is_record(Metric, metric) ->
    gen_server:start_link({local, Metric#metric.name}, ?MODULE, Metric, []);
start_link(MetricName) ->
    gen_server:start_link({local, MetricName}, ?MODULE, MetricName, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Args :: atom() | #metric{}) ->
    {ok, State :: #state{}}.
init(Metric) when is_record(Metric, metric) ->
    _Name = ets:new(Metric#metric.name, [ordered_set, named_table, {keypos, #metric_data.time}]),
    State =
        case Metric#metric.period of
            undefined ->
                #state{name = Metric#metric.name};
            Period ->
                {ok, TRef} = timer:send_interval(
                    Period * 1000, {reset, Metric#metric.name, Period}
                ),
                #state{name = Metric#metric.name, period = Period, timer_ref = TRef}
        end,
    {ok, _} = case Metric#metric.show_every of
        undefined ->
            {ok, ignore};
        Interval ->
            timer:send_interval(Interval * 1000, show)
    end,
    {ok, State};
init(Metric) ->
    _Name = ets:new(Metric, [ordered_set, named_table, {keypos, #metric_data.time}]),
    {ok, #state{name = Metric}}.

-spec handle_call(
    Request :: term(),
    From :: {pid(), Tag :: term()},
    State :: #state{}
) ->
    {reply, Reply :: term(), NewState :: #state{}}.
handle_call(get, _From, #state{name = MetricName, period = Period} = State) ->
    Count = count(MetricName, Period),
    {reply, Count, State};
handle_call({get, Period}, _From, #state{name = MetricName} = State) ->
    Count = count(MetricName, Period),
    {reply, Count, State};
handle_call({avg, Period}, _From, #state{name = MetricName} = State) ->
    Count = avg(MetricName, Period),
    {reply, Count, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}}.
handle_cast({increment, Value}, State) ->
    Key = erlang:system_time(second),
    _ = case ets:lookup(State#state.name, Key) of
        [] ->
            ets:insert(State#state.name, #metric_data{time = Key, value = Value, counter = 1});
        _ ->
            ets:update_counter(State#state.name, Key, [{#metric_data.value, Value}, {#metric_data.counter, 1}])
    end,
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}}.
handle_info(show, State) ->
    _Count = count(State#state.name, State#state.period),
    {noreply, State};
handle_info({reset, MetricName, Period}, State) ->
    StartTime = erlang:system_time(second) - Period,
    MS = [{{'_', '$1', '$2'}, [{'<', '$1', StartTime}], [true]}],
    _Result = ets:select_delete(MetricName, MS),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(
    Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}
) -> term().
terminate(_Reason, _State) ->
    ok.

-spec code_change(
    OldVsn :: term() | {down, term()},
    State :: #state{},
    Extra :: term()
) ->
    {ok, NewState :: #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

count(MetricName, Period) ->
    case Period =/= undefined of
        true ->
            StartTime = erlang:system_time(second) - Period,
            MS = [{#metric_data{time = '$1', value = '$2', counter = '$3'}, [{'>=', '$1', StartTime}], ['$_']}],
            Selection = ets:select(MetricName, MS),
            lists:foldl(fun(#metric_data{value = V}, Acc) -> Acc + V end, 0, Selection);
        false ->
            ets:foldl(fun(#metric_data{value = V}, Acc) -> Acc + V end, 0, MetricName)
    end.

avg(MetricName, Period) ->
    case Period =/= undefined of
        true ->
            StartTime = erlang:system_time(second) - Period,
            MS = [{#metric_data{time = '$1', value = '$2', counter = '$3'}, [{'>=', '$1', StartTime}], ['$_']}],
            Selection = ets:select(MetricName, MS),
            {Total, Counter} = lists:foldl(fun(#metric_data{value = V, counter = C}, {SumV, SumC}) -> {SumV + V, SumC + C} end, {0, 0}, Selection),
            calculate_avg(Total, Counter);
        false ->
            {Total, Counter} = ets:foldl(fun(#metric_data{value = V, counter = C}, {SumV, SumC}) -> {SumV + V, SumC + C} end, {0, 0}, MetricName),
            calculate_avg(Total, Counter)
    end.

calculate_avg(0, _) ->
    0;
calculate_avg(_, 0) ->
    undefined;
calculate_avg(A, B) ->
    A/B.