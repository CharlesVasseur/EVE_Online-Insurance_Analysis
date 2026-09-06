source("R/shared_functions/load_data.R")
source("R/shared_functions/loss_ratio_functions.R")
source("R/shared_functions/war_dates_function.R")

hull_value_window_a <- fread("data/model_output/hull_value_window_a.csv")
hull_value_window_b <- fread("data/model_output/hull_value_window_b.csv")
hull_value_window_a$date <- as.Date(hull_value_window_a$date)
hull_value_window_b$date <- as.Date(hull_value_window_b$date)

sde_types_window_a_classified <- fread("data/aggregated/sde_types_window_a_classified.csv")
sde_types_window_b_classified <- fread("data/aggregated/sde_types_window_b_classified.csv")

### Dreadnought payout rate

dreadnought_real_rate <- 0.15

apply_dreadnought_adjustment <- function(hull_values, dreadnought_ids, real_rate) {
  hv <- copy(as.data.table(hull_values))
  hv[, adjusted_payout := ifelse(ship_type_id %in% dreadnought_ids,
                                 estimated_hull_value * real_rate,
                                 estimated_hull_value)]
  hv
}

hv_adj_a <- apply_dreadnought_adjustment(hull_value_window_a, dreadnought_ids_a, dreadnought_real_rate)
hv_adj_b <- apply_dreadnought_adjustment(hull_value_window_b, dreadnought_ids_b, dreadnought_real_rate)

### Weekly
baseline_weekly_a <- compute_loss_ratio(aggregate_weekly(hv_adj_a, "estimated_hull_value"))
adjusted_weekly_a <- compute_loss_ratio(aggregate_weekly(hv_adj_a, "adjusted_payout"))
baseline_weekly_b <- compute_loss_ratio(aggregate_weekly(hv_adj_b, "estimated_hull_value"))
adjusted_weekly_b <- compute_loss_ratio(aggregate_weekly(hv_adj_b, "adjusted_payout"))

comparison_weekly_a <- merge(baseline_weekly_a[, .(week_start, baseline_ratio = loss_ratio)],
                             adjusted_weekly_a[, .(week_start, adjusted_ratio = loss_ratio)],
                             by = "week_start")
comparison_weekly_b <- merge(baseline_weekly_b[, .(week_start, baseline_ratio = loss_ratio)],
                             adjusted_weekly_b[, .(week_start, adjusted_ratio = loss_ratio)],
                             by = "week_start")

fwrite(comparison_weekly_a, "data/model_output/dreadnought_scenario_weekly_window_a.csv")
fwrite(comparison_weekly_b, "data/model_output/dreadnought_scenario_weekly_window_b.csv")

### Daily
baseline_daily_a <- compute_loss_ratio_daily(aggregate_daily(hv_adj_a, "estimated_hull_value"))
adjusted_daily_a <- compute_loss_ratio_daily(aggregate_daily(hv_adj_a, "adjusted_payout"))
baseline_daily_b <- compute_loss_ratio_daily(aggregate_daily(hv_adj_b, "estimated_hull_value"))
adjusted_daily_b <- compute_loss_ratio_daily(aggregate_daily(hv_adj_b, "adjusted_payout"))

comparison_daily_a <- merge(baseline_daily_a[, .(date, baseline_ratio = loss_ratio)],
                            adjusted_daily_a[, .(date, adjusted_ratio = loss_ratio)],
                            by = "date")
comparison_daily_b <- merge(baseline_daily_b[, .(date, baseline_ratio = loss_ratio)],
                            adjusted_daily_b[, .(date, adjusted_ratio = loss_ratio)],
                            by = "date")

fwrite(comparison_daily_a, "data/model_output/dreadnought_scenario_daily_window_a.csv")
fwrite(comparison_daily_b, "data/model_output/dreadnought_scenario_daily_window_b.csv")

### War-specific comparison
compute_dreadnought_gap <- function(war_dates, comparison_daily, window_id) {
  wd <- war_dates[window == window_id]
  rbindlist(lapply(seq_len(nrow(wd)), function(i) {
    w <- wd[i]
    during <- comparison_daily[date >= w$start_date & date <= w$end_date]
    data.table(
      war_name           = w$war_name,
      days_available     = nrow(during),
      baseline_avg_ratio = mean(during$baseline_ratio),
      adjusted_avg_ratio = mean(during$adjusted_ratio),
      gap                = mean(during$adjusted_ratio) - mean(during$baseline_ratio),
      pct_gap            = (mean(during$adjusted_ratio) - mean(during$baseline_ratio)) / mean(during$baseline_ratio)
    )
  }))
}

dreadnought_gap_a <- compute_dreadnought_gap(war_dates, comparison_daily_a, "a")
dreadnought_gap_b <- compute_dreadnought_gap(war_dates, comparison_daily_b, "b")

dreadnought_gap_a
dreadnought_gap_b

fwrite(rbind(dreadnought_gap_a, dreadnought_gap_b), "data/model_output/dreadnought_scenario_war_summary.csv")

setnames(dreadnought_gap_a, c("baseline_avg_ratio", "adjusted_avg_ratio"),
         c("unadjusted_avg_ratio", "dreadnought_adjusted_avg_ratio"))
setnames(dreadnought_gap_b, c("baseline_avg_ratio", "adjusted_avg_ratio"),
         c("unadjusted_avg_ratio", "dreadnought_adjusted_avg_ratio"))
