library(data.table)
library(yaml)

dir.create("data/aggregated", recursive = TRUE, showWarnings = FALSE)

`%||%` <- function(a, b) if (is.null(a)) b else a

get_manufacturing_materials <- function(bp) {
  acts <- bp$activities
  if (is.null(acts)) return(NULL)
  if (!is.null(acts$manufacturing)) return(acts$manufacturing$materials)
  if (!is.null(acts[["1"]])) return(acts[["1"]]$materials)
  return(NULL)
}

get_manufacturing_product_id <- function(bp) {
  acts <- bp$activities
  if (is.null(acts)) return(NA_integer_)
  products <- if (!is.null(acts$manufacturing)) acts$manufacturing$products
  else if (!is.null(acts[["1"]])) acts[["1"]]$products
  else NULL
  if (is.null(products) || length(products) == 0) return(NA_integer_)
  
  first <- products[[1]]
  if (!is.null(names(products)) && names(products)[1] != "") {
    as.integer(names(products)[1])
  } else {
    as.integer(first$typeID)
  }
}

flatten_blueprints <- function(blueprints_list, snapshot_label) {
  skipped_no_materials <- 0L
  skipped_no_product <- 0L
  rows <- lapply(names(blueprints_list), function(bp_id) {
    bp <- blueprints_list[[bp_id]]
    mats <- get_manufacturing_materials(bp)
    if (is.null(mats) || length(mats) == 0) {
      skipped_no_materials <<- skipped_no_materials + 1L
      return(NULL)
    }
    product_id <- get_manufacturing_product_id(bp)
    if (is.na(product_id)) {
      skipped_no_product <<- skipped_no_product + 1L
      return(NULL)
    }
    if (!is.null(names(mats)) && all(names(mats) != "")) {
      material_ids <- as.integer(names(mats))
      quantities <- sapply(mats, function(m) m$quantity)
    } else {
      material_ids <- sapply(mats, function(m) m$typeID)
      quantities <- sapply(mats, function(m) m$quantity)
    }
    data.table(
      type_id = product_id,                # CHANGED: was as.integer(bp_id)
      blueprint_id = as.integer(bp_id),     # NEW: keep original blueprint ID for traceability
      material_type_id = as.integer(material_ids),
      quantity = as.numeric(quantities),
      snapshot = snapshot_label
    )
  })
  message(sprintf("  %s: %d blueprints kept, %d skipped (no materials), %d skipped (no product)",
                  snapshot_label, length(blueprints_list) - skipped_no_materials - skipped_no_product,
                  skipped_no_materials, skipped_no_product))
  rbindlist(rows, fill = TRUE)
}

flatten_types_fast <- function(types_list, snapshot_label) {
  ids <- names(types_list)
  names_vec <- sapply(types_list, function(t) {
    n <- t$name$en %||% t$name %||% NA
    if (is.list(n)) NA else n
  })
  group_vec <- sapply(types_list, function(t) t$groupID %||% NA)
  data.table(type_id = as.integer(ids), type_name = names_vec, group_id = as.integer(group_vec), snapshot = snapshot_label)
}

bp_a <- readRDS("data/processed/sde/window_a_2014_blueprints.rds")
types_a <- readRDS("data/processed/sde/window_a_2014_types.rds")

blueprints_flat_a <- flatten_blueprints(bp_a, "2014-07-10")
types_flat_a <- flatten_types_fast(types_a, "2014-07-10")

fwrite(blueprints_flat_a, "data/aggregated/sde_blueprints_window_a.csv")
fwrite(types_flat_a, "data/aggregated/sde_types_window_a.csv")
message(sprintf("Window A: %d blueprint-material rows, %d type rows", nrow(blueprints_flat_a), nrow(types_flat_a)))

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

losses_a <- fread("data/aggregated/daily_losses_window_a.csv")
losses_b <- fread("data/aggregated/daily_losses_window_b.csv")
relevant_ship_ids <- unique(c(losses_a$ship_type_id, losses_b$ship_type_id))
message(sprintf("%d distinct ship types appear in loss data", length(relevant_ship_ids)))

types_a_full <- fread("data/aggregated/sde_types_window_a.csv")
types_a_filtered <- types_a_full[type_id %in% relevant_ship_ids]
fwrite(types_a_filtered, "data/aggregated/sde_types_window_a.csv")

types_b_full <- fread("data/aggregated/sde_types_window_b.csv")
types_b_filtered <- types_b_full[type_id %in% relevant_ship_ids]
fwrite(types_b_filtered, "data/aggregated/sde_types_window_b.csv")

message(sprintf("Window A types: %d -> %d rows | Window B types: %d -> %d rows",
                nrow(types_a_full), nrow(types_a_filtered), nrow(types_b_full), nrow(types_b_filtered)))