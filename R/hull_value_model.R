source("R/load_data.R")

### As-of join

build_hull_values <- function(losses, blueprints, prices) {
  losses <- as.data.table(losses)
  blueprints <- as.data.table(blueprints)
  prices <- as.data.table(prices)
  
  snap_dates <- unique(blueprints[, .(snapshot)])[order(snapshot)]
  setkey(snap_dates, snapshot)
  snapshot_lookup <- snap_dates[losses, on = c(snapshot = "date"), roll = TRUE,
                                j = list(snapshot_used = x.snapshot)]
  losses[, snapshot_used := snapshot_lookup$snapshot_used]
  
  earliest_snapshot <- min(snap_dates$snapshot)
  losses[is.na(snapshot_used), snapshot_used := earliest_snapshot]
  
  losses[, ship_type_id_keep := ship_type_id]
  
  setkey(blueprints, type_id, snapshot)
  mat_level <- blueprints[losses, on = c(type_id = "ship_type_id", snapshot = "snapshot_used"),
                          allow.cartesian = TRUE]
  
  prices_small <- prices[, .(type_id, date, average)]
  setkey(prices_small, type_id, date)
  mat_level <- prices_small[mat_level, on = c(type_id = "material_type_id", date = "date"), roll = TRUE]
  setnames(mat_level, "average", "price")
  
  hull_value <- mat_level[, .(
    estimated_hull_value = sum(quantity * price, na.rm = TRUE),
    n_materials_total    = .N,
    n_materials_priced   = sum(!is.na(price)),
    losses               = first(losses)
  ), by = .(date, ship_type_id = ship_type_id_keep)]
  
  hull_value[, pct_value_priced := n_materials_priced / n_materials_total]
  hull_value
}

hull_value_window_a <- build_hull_values(daily_losses_window_a, sde_blueprints_window_a, material_prices_window_a)

hull_value_window_b <- build_hull_values(daily_losses_window_b, sde_blueprints_window_b, material_prices_window_b)

### Excluded ships

exclude_ids_a <- hull_value_window_a[n_materials_priced == 0, unique(ship_type_id)]
exclude_ids_b <- hull_value_window_b[n_materials_priced == 0, unique(ship_type_id)]

excluded_summary_a <- hull_value_window_a[ship_type_id %in% exclude_ids_a,
                                          .(total_losses = sum(losses)), by = ship_type_id] %>%
  merge(unique(sde_types_window_b[, .(type_id, type_name)]), by.x = "ship_type_id", by.y = "type_id", all.x = TRUE) %>%
  arrange(desc(total_losses))

excluded_summary_b <- hull_value_window_b[ship_type_id %in% exclude_ids_b,
                                          .(total_losses = sum(losses)), by = ship_type_id] %>%
  merge(unique(sde_types_window_b[, .(type_id, type_name)]), by.x = "ship_type_id", by.y = "type_id", all.x = TRUE) %>%
  arrange(desc(total_losses))

hull_value_window_a_clean <- hull_value_window_a[!ship_type_id %in% exclude_ids_a]
hull_value_window_b_clean <- hull_value_window_b[!ship_type_id %in% exclude_ids_b]

### Outputs

dir.create("data/model_output", recursive = TRUE, showWarnings = FALSE)
fwrite(hull_value_window_a_clean, "data/model_output/hull_value_window_a.csv")
fwrite(hull_value_window_b_clean, "data/model_output/hull_value_window_b.csv")
fwrite(excluded_summary_a, "data/model_output/excluded_ship_types_window_a.csv")
fwrite(excluded_summary_b, "data/model_output/excluded_ship_types_window_b.csv")
