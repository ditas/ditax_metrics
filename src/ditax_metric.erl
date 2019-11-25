%%%-------------------------------------------------------------------
%%% @author dmitry.pravosudov
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Nov 2019 14:09
%%%-------------------------------------------------------------------
-module(ditax_metric).
-author("dmitry.pravosudov").

-include_lib("../include/ditax_metrics.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    name,
    period,
    timer_ref
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
start_link(Metric) when is_record(Metric, metric) ->
    gen_server:start_link({local, Metric#metric.name}, ?MODULE, Metric, []);
start_link(MetricName) ->
    gen_server:start_link({local, MetricName}, ?MODULE, MetricName, []).

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
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init(Metric) when is_record(Metric, metric) ->
    
    io:format("--------INIT 1 ~p ~p~n", [?MODULE, Metric]),
    
    ets:new(Metric#metric.name, [ordered_set, named_table, {keypos, #metric_data.time}]),
    State = case Metric#metric.period of
        undefined ->
            #state{name = Metric#metric.name};
        Period ->
            {ok, TRef} = timer:send_interval(Period * 1000, {reset, Metric#metric.name, Period}),
            #state{name = Metric#metric.name, period = Period, timer_ref = TRef}
    end,
    case Metric#metric.show_every of
        undefined ->
            ignore;
        Interval ->
            {ok, _} = timer:send_interval(Interval * 1000, show)
    end,
    {ok, State};
init(Metric) ->
    
    io:format("--------INIT 2 ~p ~p ~p~n", [is_record(Metric, metric), ?MODULE, Metric]),
    
    ets:new(Metric, [ordered_set, named_table, {keypos, #metric_data.time}]),
    {ok, #state{name = Metric}}.

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
handle_call(get, _From, #state{name = MetricName, period = Period} = State) ->
    Count = count(MetricName, Period),
    {reply, Count, State};
handle_call({get, Period}, _From, #state{name = MetricName} = State) ->
    Count = count(MetricName, Period),
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
handle_cast({increment, Value}, State) ->
    Key = erlang:system_time(second),
    case ets:lookup(State#state.name, Key) of
        [] ->
            ets:insert(State#state.name, #metric_data{time = Key, value = Value});
        _ ->
            _Result = ets:update_counter(State#state.name, Key, {#metric_data.value, Value})
    end,
    {noreply, State};
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
handle_info(show, State) ->
    Count = count(State#state.name, State#state.period),
    
    io:format("~n~n-----------COUNT ~p----------- ~p~n", [State#state.name, Count]),
    
    {noreply, State};
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

count(MetricName, Period) ->
    case Period =/= undefined of
        true ->
            StartTime = erlang:system_time(second) - Period,
            MS = [{#metric_data{time = '$1',value = '$2'},
                [{'>=','$1',StartTime}],
                ['$_']}],
            Selection = ets:select(MetricName, MS),
            lists:foldl(fun(#metric_data{value = V}, Acc) -> Acc + V end, 0, Selection);
        false ->
            ets:foldl(fun(#metric_data{value = V}, Acc) -> Acc + V end, 0, MetricName)
    end.