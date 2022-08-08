-module(ditax_metrics_test).
-author("ditas").

-include_lib("../include/ditax_metrics.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(METRIC_NAME, test).
-define(METRIC_NAME1, test1).
-define(METRIC_NAME2, test2).
-define(METRIC_PERIOD, 1).
-define(GET_PERIOD, 2).
-define(TIMES, 3).

metric_name_only_test() ->
    {ok, _} = ditax_metrics:start_link([?METRIC_NAME]),

    lists:foreach(fun(_) ->
        ditax_metrics:inc(?METRIC_NAME),
        timer:sleep(100)
                  end, lists:seq(1, ?TIMES)),

    TestResult = ditax_metrics:get(?METRIC_NAME),

    ?TIMES = TestResult.

metric_record_test() ->
    ditax_metrics:add(#metric{name = ?METRIC_NAME1}),

    lists:foreach(fun(_) ->
        ditax_metrics:inc(?METRIC_NAME1),
        timer:sleep(100)
                  end, lists:seq(1, ?TIMES)),

    TestResult = ditax_metrics:get(?METRIC_NAME1),

    ?TIMES = TestResult.

metric_record_with_period_test() ->
    ditax_metrics:add(#metric{name = ?METRIC_NAME2, period = ?METRIC_PERIOD}),

    lists:foreach(fun(_) ->
        ditax_metrics:inc(?METRIC_NAME2)
                  end, lists:seq(1, ?TIMES)),

    TestResult = ditax_metrics:get(?METRIC_NAME2),

    ?TIMES = TestResult,

    timer:sleep(3000),

    ditax_metrics:inc(?METRIC_NAME2),

    timer:sleep(1000),

    ditax_metrics:inc(?METRIC_NAME2),

    TestResult1 = ditax_metrics:get(?METRIC_NAME2, ?GET_PERIOD),

    2 = TestResult1.

metric_not_existing_get_test() ->
    {error, wrong_metric} = ditax_metrics:get(unknown_metric).