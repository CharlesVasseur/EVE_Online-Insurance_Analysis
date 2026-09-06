library(data.table)

dir.create("data/aggregated", recursive = TRUE, showWarnings = FALSE)

jita_region_id <- 10000002

get_material_ids <- function(blueprint_file) {
  bp <- fread(blueprint_file)
  unique(bp$material_type_id)
}

material_ids_a <- get_material_ids("data/aggregated/sde_blueprints_window_a.csv")
material_ids_b <- get_material_ids("data/aggregated/sde_blueprints_window_b.csv")

aggregate_market_history <- function(in_file, out_file, material_ids) {
  df <- fread(in_file)
  filtered <- df[region_id == jita_region_id & type_id %in% material_ids]
  setorder(filtered, date, type_id)
  fwrite(filtered, out_file)
  message(sprintf("%s: %d rows (from full file) -> %s", basename(in_file), nrow(filtered), out_file))
  invisible(filtered)
}

aggregate_market_history("data/processed/market_history_window_a.csv", "data/aggregated/material_prices_window_a.csv", material_ids_a)
aggregate_market_history("data/processed/market_history_window_b.csv", "data/aggregated/material_prices_window_b.csv", material_ids_b)

setdiff(material_ids_a, unique(fread("data/aggregated/material_prices_window_a.csv")$type_id))
setdiff(material_ids_b, unique(fread("data/aggregated/material_prices_window_b.csv")$type_id))
