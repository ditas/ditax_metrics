-record(metric, {
    name,
    period, %% Seconds
    show_every %% Seconds
}).

-record(metric_data, {
    time,
    value
}).