source("R/load_data.R")

sde_groups <- fread("data/aggregated/sde_groups.csv")

sde_types_window_a_classified <- sde_types_window_a[, .(type_id)] %>%
  left_join(sde_types_window_b %>% distinct(type_id, type_name, group_id), by = "type_id") %>%
  left_join(sde_groups, by = "group_id")

sde_types_window_a_classified <- unique(sde_types_window_a_classified, by = "type_id")
nrow(sde_types_window_a_classified)

n_still_missing <- sum(is.na(sde_types_window_a_classified$group_id))
message(sprintf("Window A: %d of %d ship types still unclassified after backfill",
                n_still_missing, nrow(sde_types_window_a_classified)))

sde_types_window_b_classified <- sde_types_window_b %>%
  left_join(sde_groups, by = "group_id")

n_missing_b <- sum(is.na(sde_types_window_b_classified$group_name))
message(sprintf("Window B: %d of %d ship types missing a group name",
                n_missing_b, nrow(sde_types_window_b_classified)))

fwrite(sde_types_window_a_classified, "data/aggregated/sde_types_window_a_classified.csv")
fwrite(sde_types_window_b_classified, "data/aggregated/sde_types_window_b_classified.csv")
