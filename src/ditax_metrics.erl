-module(ditax_metrics).
-author("ditas").

-include_lib("../include/ditax_metrics.hrl").

-behaviour(gen_server).

%% API
-export([
    start_link/1,
    start/1,
    add/1,
    delete/0,
    delete/1,
    inc/1,
    inc/2,
    dec/1,
    dec/2,
    get/1,
    get/2,
    avg/1,
    avg/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {
    metrics = []
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(MetricsList) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [MetricsList], []).

start(MetricsList) ->
    gen_server:start({local, ?SERVER}, ?MODULE, [MetricsList], []).

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

dec(MetricName) ->
    gen_server:cast(?SERVER, {MetricName, decrement, -1}).

dec(MetricName, Value) ->
    gen_server:cast(?SERVER, {MetricName, decrement, Value}).

get(MetricName) ->
    gen_server:call(?SERVER, {get, MetricName}).

get(MetricName, Period) ->
    gen_server:call(?SERVER, {get, MetricName, Period}).

avg(MetricName) ->
    avg(MetricName, undefined).

avg(MetricName, Period) ->
    gen_server:call(?SERVER, {avg, MetricName, Period}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([MetricsList]) ->

    logger:debug("-------------------------MetricsList ~p", [MetricsList]),

    State = lists:foldl(
        fun(M, S) ->
            case lists:member(M, S#state.metrics) of
                true -> S;
                false -> create_metric(M, S)
            end
        end,
        #state{},
        MetricsList
    ),
    {ok, State}.

-spec handle_call(
    Request :: term(),
    From :: {pid(), Tag :: term()},
    State :: #state{}
) ->
    {reply, Reply :: term(), NewState :: #state{}}.
handle_call({get, MetricName}, _From, State) ->
    Res =
        case lists:member(MetricName, State#state.metrics) of
            true ->
                gen_server:call(MetricName, get);
            _ ->
                logger:error("~p table does not exist ~p", [MetricName, State#state.metrics]),
                {error, wrong_metric}
        end,
    {reply, Res, State};
handle_call({get, MetricName, Period}, _From, State) ->
    Res =
        case lists:member(MetricName, State#state.metrics) of
            true ->
                gen_server:call(MetricName, {get, Period});
            _ ->
                logger:error("~p table does not exist", [MetricName]),
                {error, wrong_metric}
        end,
    {reply, Res, State};
handle_call({avg, MetricName, Period}, _From, State) ->
    Res =
        case lists:member(MetricName, State#state.metrics) of
            true ->
                gen_server:call(MetricName, {avg, Period});
            _ ->
                logger:error("~p table does not exist", [MetricName]),
                {error, wrong_metric}
        end,
    {reply, Res, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}}.
handle_cast({add, #metric{name = MetricName} = Metric}, State) ->
    State1 =
        case lists:member(MetricName, State#state.metrics) of
            false ->
                create_metric(Metric, State);
            _ ->
                logger:error("~p table already exists", [MetricName]),
                State
        end,
    {noreply, State1};
handle_cast({add, MetricName}, State) ->
    State1 =
        case lists:member(MetricName, State#state.metrics) of
            false ->
                create_metric(MetricName, State);
            _ ->
                logger:error("~p table already exists", [MetricName]),
                State
        end,
    {noreply, State1};
handle_cast({MetricName, increment, Value}, State) ->
    % logger:debug("--------2-------INC ~p", [MetricName]),
    case lists:member(MetricName, State#state.metrics) of
        true ->
            % logger:debug("--------3-------INC ~p", [MetricName]),
            gen_server:cast(MetricName, {increment, Value});
        _ ->
            % logger:debug("--------4-------INC ~p", [MetricName]),
            logger:error("~p table does not exist", [MetricName])
    end,
    {noreply, State};
handle_cast({MetricName, decrement, Value}, State) ->
    case lists:member(MetricName, State#state.metrics) of
        true when Value < 0 ->
            gen_server:cast(MetricName, {increment, Value});
        true ->
            gen_server:cast(MetricName, {increment, Value * -1});
        _ ->
            logger:error("~p table does not exist", [MetricName])
    end,
    {noreply, State};
handle_cast(delete, State) ->
    State1 = lists:foldl(
        fun(M, S) ->
            ok = gen_server:cast(M, {delete, M}),
            CurrentMetrics = S#state.metrics,
            S#state{metrics = lists:delete(M, CurrentMetrics)}
        end,
        State,
        State#state.metrics
    ),
    {noreply, State1};
handle_cast({delete, MetricName}, State) ->
    State1 =
        case lists:member(MetricName, State#state.metrics) of
            true ->
                ok = gen_server:cast(MetricName, {delete, MetricName}),
                CurrentMetrics = State#state.metrics,
                State#state{metrics = lists:keydelete(MetricName, 1, CurrentMetrics)};
            _ ->
                logger:error("~p table does not exist", [MetricName]),
                State
        end,
    {noreply, State1};
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}}.
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

create_metric(#metric{name = MetricName} = Metric, State) ->

    logger:debug("------------Metric ~p", [Metric]),

    CurrentMetrics = State#state.metrics,
    {ok, _Pid} = ditax_metric:start_link(Metric),
    State#state{metrics = [MetricName | CurrentMetrics]};
create_metric(MetricName, State) ->

    logger:debug("------------MetricName ~p", [MetricName]),

    CurrentMetrics = State#state.metrics,
    {ok, _Pid} = ditax_metric:start_link(MetricName),
    State#state{metrics = [MetricName | CurrentMetrics]}.
