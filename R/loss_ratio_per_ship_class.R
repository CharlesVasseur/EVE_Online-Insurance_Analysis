source("R/shared_functions.R")

hull_value_window_a <- fread("data/model_output/hull_value_window_a.csv")
hull_value_window_b <- fread("data/model_output/hull_value_window_b.csv")
hull_value_window_a$date <- as.Date(hull_value_window_a$date)
hull_value_window_b$date <- as.Date(hull_value_window_b$date)

sde_types_window_a_classified <- fread("data/aggregated/sde_types_window_a_classified.csv")
sde_types_window_b_classified <- fread("data/aggregated/sde_types_window_b_classified.csv")

combat_classes <- c("Frigate", "Cruiser", "Battleship", "Destroyer",
                    "Dreadnought", "Carrier", "Titan", "Supercarrier")

attach_class <- function(hv, classified) {
  merge(hv, classified[, .(type_id, group_name)], by.x = "ship_type_id", by.y = "type_id", all.x = TRUE)
}

hv_a_classed <- attach_class(hull_value_window_a, sde_types_window_a_classified)
hv_b_classed <- attach_class(hull_value_window_b, sde_types_window_b_classified)

aggregate_monthly_by_class <- function(hv, classes) {
  hv <- hv[group_name %in% classes]
  hv[, month_start := floor_date(date, unit = "month")]
  hv[, .(total_payout = sum(estimated_hull_value)), by = .(month_start, group_name)]
}

compute_loss_ratio_by_class <- function(monthly, window_months = 3) {
  setorder(monthly, group_name, month_start)
  monthly[, premium := shift(frollmean(total_payout, n = window_months, align = "right"), n = 1), by = group_name]
  monthly[, loss_ratio := total_payout / premium]
  monthly[!is.na(loss_ratio)]
}

class_ratio_a <- compute_loss_ratio_by_class(aggregate_monthly_by_class(hv_a_classed, combat_classes))
class_ratio_b <- compute_loss_ratio_by_class(aggregate_monthly_by_class(hv_b_classed, combat_classes))

fwrite(class_ratio_a, "data/model_output/loss_ratio_by_class_window_a.csv")
fwrite(class_ratio_b, "data/model_output/loss_ratio_by_class_window_b.csv")