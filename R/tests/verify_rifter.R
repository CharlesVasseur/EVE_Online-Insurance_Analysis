source("R/load_data.R")

rifter_bp_a <- sde_blueprints_window_a %>% filter(type_id == 587)
rifter_bp_a

rifter_bp_b <- sde_blueprints_window_b %>% filter(type_id == 587)
rifter_bp_b %>% distinct(snapshot) %>% arrange(snapshot)

rifter_bp_b_one <- sde_blueprints_window_b %>%
  filter(type_id == 587, snapshot == as.Date("2018-01-10"))
rifter_bp_b_one

mineral_ids <- c(34, 35, 36, 37, 38, 39, 40, 11399)
rifter_bp_a %>% mutate(is_mineral = material_type_id %in% mineral_ids)
rifter_bp_b_one %>% mutate(is_mineral = material_type_id %in% mineral_ids)

rifter_losses_a <- daily_losses_window_a %>% filter(ship_type_id == 587) %>% arrange(date)
head(rifter_losses_a)

trace_date <- as.Date("2011-01-01")

get_price_asof <- function(mat_id, asof_date, price_table) {
  price_table %>%
    filter(type_id == mat_id, date <= asof_date) %>%
    arrange(desc(date)) %>%
    slice(1) %>%
    pull(average)
}

rifter_priced_a <- rifter_bp_a %>%
  rowwise() %>%
  mutate(price = get_price_asof(material_type_id, trace_date, material_prices_window_a),
         line_value = quantity * price) %>%
  ungroup()

rifter_priced_a
sum(rifter_priced_a$line_value, na.rm = TRUE)
