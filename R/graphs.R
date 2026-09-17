library(ggplot2)

source("R/shared_functions.R")

loss_ratio_window_a <- fread("data/model_output/loss_ratio_weekly_window_a.csv")
loss_ratio_window_b <- fread("data/model_output/loss_ratio_weekly_window_b.csv")
loss_ratio_daily_a  <- fread("data/model_output/loss_ratio_daily_window_a.csv")
loss_ratio_daily_b  <- fread("data/model_output/loss_ratio_daily_window_b.csv")
war_case_studies_daily <- fread("data/model_output/war_case_studies_daily.csv")
mer_cross_check <- fread("data/model_output/mer_cross_check_daily.csv")
dreadnought_daily_a <- fread("data/model_output/dreadnought_scenario_daily_window_a.csv")
dreadnought_daily_b <- fread("data/model_output/dreadnought_scenario_daily_window_b.csv")
loss_ratio_by_class_window_a <- fread("data/model_output/loss_ratio_by_class_window_a.csv")
loss_ratio_by_class_window_b <- fread("data/model_output/loss_ratio_by_class_window_b.csv")

dreadnought_daily_a$date <- as.Date(dreadnought_daily_a$date)
dreadnought_daily_b$date <- as.Date(dreadnought_daily_b$date)

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

### Theme

theme_project <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())

war_color <- "#B22222"

window_labels <- c(a = "Window A (2011-2015)", b = "Window B (2018-2022)")

### Graph 1: Weekly Loss Ratio Overview

loss_ratio_window_a[, window := "a"]
loss_ratio_window_b[, window := "b"]
combined_weekly <- rbind(loss_ratio_window_a, loss_ratio_window_b)

graph_1 <- ggplot(combined_weekly, aes(x = week_start, y = loss_ratio)) +
  geom_rect(data = war_dates, inherit.aes = FALSE,
            aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
            fill = war_color, alpha = 0.15) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  geom_line(color = "grey20", linewidth = 0.4) +
  facet_wrap(~window, scales = "free_x", labeller = labeller(window = window_labels)) +
  labs(title = "Modeled Insurance Loss Ratio Over Time",
       subtitle = "Shaded bands mark documented wars. Dashed line = break-even (ratio = 1).",
       x = NULL, y = "Loss ratio") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_project

graph_1
ggsave("output/figures/loss_ratio_overview.png", graph_1, width = 12, height = 6)

### Graph 2: Daily Zoom Loss Ratio

zoom_window <- function(daily, event_date, buffer_days = 14, event_label) {
  daily[date >= event_date - buffer_days & date <= event_date + buffer_days][, event := event_label]
}

zoom_bloodbath <- zoom_window(loss_ratio_daily_a, as.Date("2014-01-27"), event_label = "Bloodbath of B-R5RB")
zoom_massacre  <- zoom_window(loss_ratio_daily_b, as.Date("2020-12-30"), event_label = "Massacre at M2-XFE")

zoom_combined <- rbind(zoom_bloodbath, zoom_massacre)
event_dates <- data.table(
  event = c("Bloodbath of B-R5RB", "Massacre at M2-XFE"),
  start_date = as.Date(c("2014-01-27", "2020-12-30")),
  end_date   = as.Date(c("2014-01-27", "2020-12-31"))
)

graph_2 <- ggplot(zoom_combined, aes(x = date, y = loss_ratio)) +
  geom_rect(data = event_dates, inherit.aes = FALSE,
            aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
            fill = war_color, alpha = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  geom_line(color = "grey20", linewidth = 0.6) +
  geom_point(color = "grey20", size = 1.5) +
  facet_wrap(~event, scales = "free_x") +
  labs(title = "Daily Loss Ratio Around Short, Sudden War Events",
       subtitle = "Weekly resolution misses these entirely - daily data reveals the spike.",
       x = NULL, y = "Loss ratio (daily)") +
  geom_vline(data = event_dates[start_date == end_date],
             aes(xintercept = start_date), color = war_color, linewidth = 0.8) +
  theme_project

  
graph_2

ggsave("output/figures/short_war_zoom.png", graph_2, width = 10, height = 5)

### Graph 3: War Case Study Summary

war_summary_long <- melt(war_case_studies_daily,
                         id.vars = "war_name",
                         measure.vars = c("baseline_avg_ratio", "war_avg_ratio", "war_peak_ratio"),
                         variable.name = "metric", value.name = "ratio")

war_summary_long[, metric := factor(metric,
                                    levels = c("baseline_avg_ratio", "war_avg_ratio", "war_peak_ratio"),
                                    labels = c("Baseline (calm)", "War average", "War peak"))]

graph_3 <- ggplot(war_summary_long, aes(x = war_name, y = ratio, fill = metric)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  scale_fill_manual(values = c("Baseline (calm)" = "grey70",
                               "War average" = "#4472C4",
                               "War peak" = war_color)) +
  labs(title = "Loss Ratio: Baseline vs. War Average vs. War Peak",
       x = NULL, y = "Loss ratio", fill = NULL) +
  theme_project +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

graph_3
ggsave("output/figures/war_case_studies.png", graph_3, width = 9, height = 5.5)

### Graph 4: Dreadnought Adjusted Comparison

compute_payout_magnitude <- function(war_dates, comparison_daily, window_id) {
  wd <- war_dates[window == window_id]
  rbindlist(lapply(seq_len(nrow(wd)), function(i) {
    w <- wd[i]
    during <- comparison_daily[date >= w$start_date & date <= w$end_date]
    data.table(war_name = w$war_name,
               baseline_payout = mean(during$baseline_payout),
               adjusted_payout = mean(during$adjusted_payout))
  }))
}

payout_magnitude_all <- rbind(
  compute_payout_magnitude(war_dates, dreadnought_daily_a, "a"),
  compute_payout_magnitude(war_dates, dreadnought_daily_b, "b")
)

magnitude_long <- melt(payout_magnitude_all, id.vars = "war_name",
                       measure.vars = c("baseline_payout", "adjusted_payout"),
                       variable.name = "scenario", value.name = "isk")

graph_4 <- ggplot(magnitude_long, aes(x = war_name, y = isk / 1e9, fill = scenario)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  scale_fill_manual(values = c(baseline_payout = "grey70", adjusted_payout = "#4472C4"),
                    labels = c("Baseline (100%)", "Dreadnought-adjusted (~15%)")) +
  labs(title = "Modeled Insurer Liability: Baseline vs. Dreadnought-Adjusted",
       subtitle = "Average daily payout during each war, billion ISK",
       x = NULL, y = "Avg. daily payout (billion ISK)", fill = NULL) +
  theme_project + theme(axis.text.x = element_text(angle = 20, hjust = 1)) + 
  geom_text(aes(label = round(isk / 1e9, 1)),
            position = position_dodge(width = 0.75),
            vjust = -0.4, size = 3.2, color = "grey20")

graph_4
ggsave("output/figures/dreadnought_scenario.png", graph_4, width = 9, height = 5.5)

### Graph 5: MER cross-check

mer_indexed <- copy(mer_cross_check)
mer_indexed[, modeled_index := modeled_adjusted_payout / mean(modeled_adjusted_payout, na.rm = TRUE)]
mer_indexed[, real_index    := insurance_net_isk / mean(insurance_net_isk, na.rm = TRUE)]

mer_indexed[, modeled_smooth := frollmean(modeled_index, 30, align = "right")]
mer_indexed[, real_smooth := frollmean(real_index, 30, align = "right")]

mer_indexed[, real_smooth_capped := pmax(real_smooth, -2)]

mer_smooth_long <- melt(mer_indexed, id.vars = "date",
                        measure.vars = c("modeled_smooth", "real_smooth_capped"),
                        variable.name = "series", value.name = "index")

graph_5 <- ggplot(mer_smooth_long, aes(x = date, y = index, color = series)) +
  geom_line(linewidth = 0.6) +
  labs(title = "Modeled Payout vs. Real MER Net Insurance (30-day smoothed)",
       subtitle = "r \u2248 0.14 (daily)",
       x = NULL, y = "Index (mean = 1.0)", color = NULL) +
  theme_project

graph_5
ggsave("output/figures/mer_cross_check.png", graph_5, width = 9, height = 5.5)

### Graph 6: Loss Ratio per ship Class

loss_ratio_by_class_window_a[, window := "a"]
loss_ratio_by_class_window_b[, window := "b"]
combined_class <- rbind(loss_ratio_by_class_window_a, loss_ratio_by_class_window_b)

graph_6 <- ggplot(combined_class, aes(x = month_start, y = loss_ratio)) +
  geom_rect(data = war_dates, inherit.aes = FALSE,
            aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
            fill = war_color, alpha = 0.12) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  geom_line(color = "grey20", linewidth = 0.4) +
  facet_grid(group_name ~ window, scales = "free", space = "free_x",
             labeller = labeller(window = window_labels)) +
  labs(title = "Loss Ratio by Ship Class",
       subtitle = "Monthly resolution, 3-month trailing baseline - not directly comparable in scale to the fleet-wide weekly ratio",
       x = NULL, y = "Loss ratio") +
  theme_project +
  theme(strip.text.y = element_text(angle = 0, size = 8))

graph_6
ggsave("output/figures/loss_ratio_by_class.png", graph_6, width = 11, height = 10)
