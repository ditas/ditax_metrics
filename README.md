#How to use

Start the module: `custom_metrics:start_link([Metric])` where `Metric` can be just a name (atom) or a record `#metric{}`.

If you want to add some default value for the metric's reset use `#metric{period = some_value_in_seconds}`. Besides, if you have this period given, by default you will get the metric value for this exact period.

If you want to add some metrics when the module is already started use `custom_metrics:add(Metric)` where `Metric` is `atom() | #metric{name = SomeName} | #metric{name = SomeName, period = SomePeriod}`.

To add new value for the metric use `custom_metrics:inc(MetricName)` where MetricName is `atom()` or `custom_metrics:inc(MetricName, SomeValue)`.

To get the result use `custom_metrics:get(MetricName)` or `custom_metrics:get(MetricName, CustomPeriod)` where CustomPeriod is less than the default period used for creating this metric (if there is one).