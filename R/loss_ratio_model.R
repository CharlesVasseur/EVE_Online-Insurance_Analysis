source("R/load_data.R")
source("R/loss_ratio_functions.R")

hull_value_window_a <- fread("data/model_output/hull_value_window_a.csv")
hull_value_window_b <- fread("data/model_output/hull_value_window_b.csv")

hull_value_window_a$date <- as.Date(hull_value_window_a$date)
hull_value_window_b$date <- as.Date(hull_value_window_b$date)

### Weekly Loss Ratio

weekly_a <- aggregate_weekly(hull_value_window_a)
weekly_b <- aggregate_weekly(hull_value_window_b)

loss_ratio_a <- compute_loss_ratio(weekly_a)
loss_ratio_b <- compute_loss_ratio(weekly_b)

loss_ratio_a <- loss_ratio_a[!is.na(loss_ratio)]
loss_ratio_b <- loss_ratio_b[!is.na(loss_ratio)]

fwrite(loss_ratio_a, "data/model_output/loss_ratio_weekly_window_a.csv")
fwrite(loss_ratio_b, "data/model_output/loss_ratio_weekly_window_b.csv")

### Daily Loss Ratio

daily_a <- aggregate_daily(hull_value_window_a)
daily_b <- aggregate_daily(hull_value_window_b)
loss_ratio_daily_a <- compute_loss_ratio_daily(daily_a)[!is.na(loss_ratio)]
loss_ratio_daily_b <- compute_loss_ratio_daily(daily_b)[!is.na(loss_ratio)]

fwrite(loss_ratio_daily_a, "data/model_output/loss_ratio_daily_window_a.csv")
fwrite(loss_ratio_daily_b, "data/model_output/loss_ratio_daily_window_b.csv")
