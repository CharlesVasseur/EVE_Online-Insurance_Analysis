source("R/shared_functions/load_data.R")
source("R/shared_functions/loss_ratio_functions.R")
source("R/shared_functions/war_dates_function.R")

hull_value_window_b <- fread("data/model_output/hull_value_window_b.csv")
hull_value_window_b$date <- as.Date(hull_value_window_b$date)
sde_types_window_b_classified <- fread("data/aggregated/sde_types_window_b_classified.csv")

dreadnought_ids_b <- sde_types_window_b_classified[group_name == "Dreadnought", type_id]
stopifnot(length(dreadnought_ids_b) < 30)
dreadnought_real_rate <- 0.15

hv_adj_b <- copy(hull_value_window_b)
hv_adj_b[, adjusted_payout := ifelse(ship_type_id %in% dreadnought_ids_b,
                                     estimated_hull_value * dreadnought_real_rate,
                                     estimated_hull_value)]

daily_baseline <- aggregate_daily(hv_adj_b, "estimated_hull_value")
setnames(daily_baseline, "total_payout", "modeled_baseline_payout")
daily_adjusted <- aggregate_daily(hv_adj_b, "adjusted_payout")
setnames(daily_adjusted, "total_payout", "modeled_adjusted_payout")

modeled_daily <- merge(daily_baseline, daily_adjusted, by = "date")
mer_daily <- mer_insurance_window_b[, .(date, insurance_net_isk)]
comparison <- merge(modeled_daily, mer_daily, by = "date")

### MER comparison Check: Baseline vs Adjusted

cor(comparison$modeled_baseline_payout, comparison$insurance_net_isk, use = "complete.obs")
cor(comparison$modeled_adjusted_payout, comparison$insurance_net_isk, use = "complete.obs")

fwrite(comparison, "data/model_output/mer_cross_check_daily.csv")

### Correlation checks

##### Spearman correlation
cor(comparison$modeled_adjusted_payout, comparison$insurance_net_isk,
    method = "spearman", use = "complete.obs")

##### Weekly correlation
weekly_modeled <- aggregate_weekly(hv_adj_b, "adjusted_payout")
weekly_mer <- mer_insurance_window_b[, .(week_start = floor_date(date, unit = "week", week_start = 6),
                                         insurance_net_isk = sum(insurance_net_isk)), by = .(floor_date(date, unit = "week", week_start = 6))][, floor_date := NULL]
weekly_comparison <- merge(weekly_modeled, weekly_mer, by = "week_start")

cor(weekly_comparison$total_payout, weekly_comparison$insurance_net_isk, use = "complete.obs")

##### Timezone mismatch
comparison[, mer_lag1 := shift(insurance_net_isk, 1)]
comparison[, mer_lead1 := shift(insurance_net_isk, -1)]
cor(comparison$modeled_adjusted_payout, comparison$mer_lag1, use = "complete.obs")
cor(comparison$modeled_adjusted_payout, comparison$mer_lead1, use = "complete.obs")

### War-window magnitude comparison (WWB2 + Massacre)

war_dates_b <- war_dates[window == "b"]

war_window_comparison <- rbindlist(lapply(seq_len(nrow(war_dates_b)), function(i) {
  w <- war_dates_b[i]
  during <- comparison[date >= w$start_date & date <= w$end_date]
  data.table(
    war_name = w$war_name,
    days_available = nrow(during),
    avg_modeled_baseline_payout = mean(during$modeled_baseline_payout),
    avg_modeled_adjusted_payout = mean(during$modeled_adjusted_payout),
    avg_real_mer_net = mean(during$insurance_net_isk)
  )
}))

war_window_comparison
fwrite(war_window_comparison, "data/model_output/mer_cross_check_war_summary.csv")
