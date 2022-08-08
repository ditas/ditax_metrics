-module(ditax_metrics_app).

-behaviour(application).

-include_lib("../include/ditax_metrics.hrl").

%% application callbacks
-export([
    start/2,
    stop/1
]).

%%%===================================================================
%%% application callbacks
%%%===================================================================
%% @private
-spec start(
    StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()
) ->
    {ok, pid()} | {error, Reason :: term()}.
start(_StartType, _StartArgs) ->

    case application:get_env(ditax_metrics, default_metrics, undefined) of
        undefined -> ignore;
        DefaultMetrics -> ditax_metrics:start(DefaultMetrics)
    end,

    ditax_metrics_sup:start_link().

%% @private
-spec stop(State :: term()) -> ok.
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================