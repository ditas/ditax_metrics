%%%-------------------------------------------------------------------
%%% @author pravosudov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Nov 2017 12:16
%%%-------------------------------------------------------------------
-module(custom_metrics_test).
-author("pravosudov").

-include_lib("../include/common.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(METRIC_NAME, test).
-define(METRIC_NAME1, test1).
-define(METRIC_NAME2, test2).
-define(METRIC_PERIOD, 1).
-define(GET_PERIOD, 2).
-define(TIMES, 3).

metric_name_only_test() ->
    lager:start(),

    custom_metrics:start_link([?METRIC_NAME]),

    lists:foreach(fun(_) ->
        custom_metrics:inc(?METRIC_NAME),
        timer:sleep(100)
                  end, lists:seq(1, ?TIMES)),

    TestResult = custom_metrics:get(?METRIC_NAME),

    ?TIMES = TestResult.

metric_record_test() ->
    custom_metrics:add(#metric{name = ?METRIC_NAME1}),

    lists:foreach(fun(_) ->
        custom_metrics:inc(?METRIC_NAME1),
        timer:sleep(100)
                  end, lists:seq(1, ?TIMES)),

    TestResult = custom_metrics:get(?METRIC_NAME1),

    ?TIMES = TestResult.

metric_record_with_period_test() ->

%%    lager:start(),
%%    custom_metrics:start_link([#metric{name = ?METRIC_NAME2, period = ?METRIC_PERIOD}]),

    custom_metrics:add(#metric{name = ?METRIC_NAME2, period = ?METRIC_PERIOD}),

    lists:foreach(fun(_) ->
        custom_metrics:inc(?METRIC_NAME2)
                  end, lists:seq(1, ?TIMES)),

    TestResult = custom_metrics:get(?METRIC_NAME2),

    ?TIMES = TestResult,

    timer:sleep(3000),

    custom_metrics:inc(?METRIC_NAME2),

    timer:sleep(1000),

    custom_metrics:inc(?METRIC_NAME2),

    TestResult1 = custom_metrics:get(?METRIC_NAME2, ?GET_PERIOD),

    2 = TestResult1.

metric_existing_add_test() ->
    {error, table_already_exists} = custom_metrics:add(#metric{name = ?METRIC_NAME}).

metric_not_existing_get_test() ->
    {error, table_does_not_exist} = custom_metrics:get(unknown_metric).