### ditax_metrics

It is a simple set of erlang modules to collect any custom metrics from your erlang apps. Module works as erlang gen_server, which keeps all your metrics in ets tables.
You can collect metrics for any reasonable period of time. When creating a new metric to store you can set the reset period for this metric. In this case you will get the result for the predefined period (if no other period is defined). Also you can skip the value of metric so that will be always 1 or you can set it in your `inc/2` call.

### How to use

Start the module: `ditax_metrics:start_link([Metric])` where `Metric` can be just a name (atom) or a record `#metric{}`.

If you want to add some default value for the metric's reset use `#metric{period = some_value_in_seconds}`. Besides, if you have this period given, by default you will get the metric value for this exact period.

If you want to add some metrics when the module is already started use `ditax_metrics:add(Metric)` where `Metric` is `atom() | #metric{name = SomeName} | #metric{name = SomeName, period = SomePeriod}`.

To add new value for the metric use `ditax_metrics:inc(MetricName)` where MetricName is `atom()` or `ditax_metrics:inc(MetricName, SomeValue)`.

To get the result use `ditax_metrics:get(MetricName)` or `ditax_metrics:get(MetricName, CustomPeriod)` where CustomPeriod is less than the default period used for creating this metric (if there is one).
