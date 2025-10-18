
# scripts/05_buff.R
source("../R/utils.R")

inv_path <- "../out/inventory_730.csv"
if (!file.exists(inv_path)) stop("No existe out/inventory_730.csv; ejecuta 04_inventory.R")

inv_all <- readr::read_csv(inv_path, show_col_types = FALSE)

xz_path <- buff_download_weekly("../data/price-history-weekly.json.xz")
buff_weekly <- buff_read_jsonxz(xz_path)

items <- inv_all %>% dplyr::filter(!is.na(market_hash_name)) %>% dplyr::distinct(market_hash_name) %>% dplyr::pull()
BUFF_MAX_ITEMS <- as.integer(Sys.getenv("BUFF_MAX_ITEMS", "200"))
items <- head(items, BUFF_MAX_ITEMS)
message("Items a muestrear de Buff (weekly): ", length(items))

series_list <- lapply(items, function(nm) { df <- buff_series(buff_weekly, nm); df$item <- nm; df })
buff_long <- dplyr::bind_rows(series_list)

readr::write_csv(buff_long, "../out/buff_weekly_subset.csv")
saveRDS(buff_long, "../out/buff_weekly_subset.rds")
message("Guardado: out/buff_weekly_subset.csv y out/buff_weekly_subset.rds")
