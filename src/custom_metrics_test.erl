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
-define(METRIC_PERIOD, 1500).
-define(GET_PERIOD, 1000).
-define(TIMES, 3).

metric_name_only_test() ->
    lager:start(),
    custom_metrics:start_link([?METRIC_NAME]),

    lists:foreach(fun(_) ->
        custom_metrics:inc(?METRIC_NAME)
                  end, lists:seq(1, ?TIMES)),

    TestResult = custom_metrics:get(?METRIC_NAME),

    ?TIMES = TestResult.

metric_record_test() ->
    custom_metrics:add(#metric{name = ?METRIC_NAME1}),

    lists:foreach(fun(_) ->
        custom_metrics:inc(?METRIC_NAME1)
                  end, lists:seq(1, ?TIMES)),

    TestResult = custom_metrics:get(?METRIC_NAME1),

    ?TIMES = TestResult.

metric_record_with_period_test() ->
    custom_metrics:add(#metric{name = ?METRIC_NAME2, period = ?METRIC_PERIOD}),

    lists:foreach(fun(_) ->
        custom_metrics:inc(?METRIC_NAME2)
                  end, lists:seq(1, ?TIMES)),

    TestResult = custom_metrics:get(?METRIC_NAME2),

    ?TIMES = TestResult,

    timer:sleep(1750),

    custom_metrics:inc(?METRIC_NAME2),

    TestResult1 = custom_metrics:get(?METRIC_NAME2),

    1 = TestResult1,

    timer:sleep(1750),

    lists:foreach(fun(_) ->
        custom_metrics:inc(?METRIC_NAME2)
                  end, lists:seq(1, ?TIMES)),

    TestResult2 = custom_metrics:get(?METRIC_NAME2, ?GET_PERIOD),

    ?TIMES = TestResult2.
