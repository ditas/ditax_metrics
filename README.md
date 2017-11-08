<h1>How to use</h1>

<p>Start the module:<br>
<em>custom_metrics:start_link([Metric])</em><br>
where <em>Metric</em> can be just a name (atom) or a record <em>#metric{}</em>.</p>

<p>If you want to add some default value for the metric's reset use <em>#metric{period = some_value_in_milliseconds}</em>. Besides, if you have this period given, you will get the metric value for this exact period.</p>

<p>If you want to add some metrics when the module is already started use <em>custom_metrics:add(Metric)</em> where <em>Metric</em> is atom() | #metric{name = SomeName} | #metric{name = SomeName, period = SomePeriod}.</p>

<p>To add new value for the metric use <em>custom_metrics:inc(MetricName)</em> where MetricName is atom().</p>

<p>To get the result use <em>custom_metrics:get(MetricName)</em> or <em>custom_metrics:get(MetricName, CustomPeriod)</em> where CustomPeriod is less than the default period used for creating this metric (if there is one).</p>