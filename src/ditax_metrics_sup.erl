-module(ditax_metrics_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Callbacks
-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Callbacks
%%%===================================================================

%% @private
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1000,
        period => 5000
    },
    % Child = #{
    %     id => ,
    %     start => {, start_link, []},
    %     restart => permanent,
    %     shutdown => 2000,
    %     type => worker,
    %     modules => []
    % },
    ChildSpecs = [
    ],
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================