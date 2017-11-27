-record(metric, {
    name,
    period %% Seconds
}).

-record(created_metric, {
    name,
    period, %% Seconds
    timer_ref
}).

-record(metric_data, {
    time,
    value
}).