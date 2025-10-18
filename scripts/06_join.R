
# scripts/06_join.R
source("../R/utils.R")

inv_path <- "../out/inventory_730.csv"
buff_path <- "../out/buff_weekly_subset.csv"
if (!file.exists(inv_path)) stop("Falta out/inventory_730.csv (04_inventory.R)")
if (!file.exists(buff_path)) stop("Falta out/buff_weekly_subset.csv (05_buff.R)")

inv_all <- readr::read_csv(inv_path, show_col_types = FALSE)
buff_long <- readr::read_csv(buff_path, show_col_types = FALSE)

last_price <- buff_long %>%
  dplyr::group_by(item) %>%
  dplyr::filter(ts == max(ts)) %>%
  dplyr::ungroup() %>%
  dplyr::select(item, last_price_cny = price_cny, last_listings = listings)

inv_priced <- inv_all %>%
  dplyr::left_join(last_price, by = c("market_hash_name" = "item"))

readr::write_csv(inv_priced, "../out/inventory_730_priced.csv")
saveRDS(inv_priced, "../out/inventory_730_priced.rds")
message("Guardado: out/inventory_730_priced.csv y out/inventory_730_priced.rds")
