source("R/load_data.R")

aggregate_weekly <- function(hull_values) {
  hv <- as.data.table(hull_values)
  hv[, week_start := floor_date(date, unit = "week", week_start = 6)]
  hv[, .(total_payout = sum(estimated_hull_value)), by = week_start]
}

weekly_a <- aggregate_weekly(hull_value_window_a)
weekly_b <- aggregate_weekly(hull_value_window_b)

compute_loss_ratio <- function(weekly, window_weeks = 12) {
  setorder(weekly, week_start)
  weekly[, premium := shift(frollmean(total_payout, n = window_weeks, align = "right"), n = 1)]
  weekly[, loss_ratio := total_payout / premium]
  weekly
}

loss_ratio_a <- compute_loss_ratio(weekly_a)
loss_ratio_b <- compute_loss_ratio(weekly_b)

loss_ratio_a <- loss_ratio_a[!is.na(loss_ratio)]
loss_ratio_b <- loss_ratio_b[!is.na(loss_ratio)]

fwrite(loss_ratio_a, "data/aggregated/loss_ratio_window_a.csv")
fwrite(loss_ratio_b, "data/aggregated/loss_ratio_window_b.csv")
