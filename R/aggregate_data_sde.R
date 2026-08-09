library(data.table)
library(yaml)

dir.create("data/aggregated", recursive = TRUE, showWarnings = FALSE)

`%||%` <- function(a, b) if (is.null(a)) b else a

get_manufacturing_materials <- function(bp) {
  acts <- bp$activities
  if (is.null(acts)) return(NULL)  # no activities at all -- fine, skip
  if (!is.null(acts$manufacturing)) return(acts$manufacturing$materials)
  if (!is.null(acts[["1"]])) return(acts[["1"]]$materials)
  return(NULL)  # has activities, just not a manufacturing one (e.g. copy/invention-only) -- also fine
}

flatten_blueprints <- function(blueprints_list, snapshot_label) {
  skipped <- 0L
  rows <- lapply(names(blueprints_list), function(bp_id) {
    bp <- blueprints_list[[bp_id]]
    mats <- get_manufacturing_materials(bp)
    if (is.null(mats) || length(mats) == 0) {
      skipped <<- skipped + 1L
      return(NULL)
    }
    data.table(
      type_id = as.integer(bp_id),
      material_type_id = as.integer(names(mats)),
      quantity = sapply(mats, function(m) m$quantity),
      snapshot = snapshot_label
    )
  })
  message(sprintf("  %s: %d blueprints skipped (no manufacturing materials), %d kept",
                  snapshot_label, skipped, length(blueprints_list) - skipped))
  rbindlist(rows, fill = TRUE)
}

flatten_types_fast <- function(types_list, snapshot_label) {
  ids <- names(types_list)
  names_vec <- sapply(types_list, function(t) {
    n <- t$name$en %||% t$name %||% NA
    if (is.list(n)) NA else n
  })
  data.table(type_id = as.integer(ids), type_name = names_vec, snapshot = snapshot_label)
}

# --- Window A: single snapshot ---
bp_a <- readRDS("data/processed/sde/window_a_2014_blueprints.rds")
types_a <- readRDS("data/processed/sde/window_a_2014_types.rds")

blueprints_flat_a <- flatten_blueprints(bp_a, "2014-07-10")
types_flat_a <- flatten_types_fast(types_a, "2014-07-10")

fwrite(blueprints_flat_a, "data/aggregated/sde_blueprints_window_a.csv")
fwrite(types_flat_a, "data/aggregated/sde_types_window_a.csv")
message(sprintf("Window A: %d blueprint-material rows, %d type rows", nrow(blueprints_flat_a), nrow(types_flat_a)))

# --- Window B: all snapshots ---
sde_b_bp_files <- list.files("data/processed/sde", pattern = "^window_b_.*_blueprints\\.rds$", full.names = TRUE)
sde_b_types_files <- list.files("data/processed/sde", pattern = "^window_b_.*_types\\.rds$", full.names = TRUE)

blueprints_flat_b <- rbindlist(lapply(sde_b_bp_files, function(f) {
  snap_id <- gsub("^window_b_|_blueprints\\.rds$", "", basename(f))
  flatten_blueprints(readRDS(f), snap_id)
}), fill = TRUE)

types_flat_b <- rbindlist(lapply(sde_b_types_files, function(f) {
  snap_id <- gsub("^window_b_|_types\\.rds$", "", basename(f))
  flatten_types_fast(readRDS(f), snap_id)
}), fill = TRUE)

fwrite(blueprints_flat_b, "data/aggregated/sde_blueprints_window_b.csv")
fwrite(types_flat_b, "data/aggregated/sde_types_window_b.csv")
message(sprintf("Window B: %d blueprint-material rows, %d type rows across %d snapshots",
                nrow(blueprints_flat_b), nrow(types_flat_b), length(sde_b_bp_files)))
