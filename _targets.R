
# _targets.R
library(targets)
tar_option_set(packages = c("httr2","jsonlite","xml2","dplyr","purrr","tibble","tidyr",
                            "stringr","igraph","lubridate","readr"))
source("R/utils.R")

BFS_DEPTH <- as.integer(Sys.getenv("BFS_DEPTH", "2"))
MAX_NODES <- as.integer(Sys.getenv("MAX_NODES", "30000"))
BUFF_MAX_ITEMS <- as.integer(Sys.getenv("BUFF_MAX_ITEMS", "200"))

list(
  tar_target(seed_groups_file, "config/seed_groups.txt", format = "file"),
  tar_target(seed_groups, readLines(seed_groups_file, warn = FALSE)),
  tar_target(seed_ids, unique(unlist(purrr::map(seed_groups, steam_group_members)))),
  tar_target(bfs, crawl_friends_bfs(seed_ids, depth = BFS_DEPTH, max_nodes = MAX_NODES)),
  tar_target(users_cs2, has_cs2_many(unique(c(seed_ids, bfs$nodes)))),
  tar_target(inv_all, {
    keep <- users_cs2 |> dplyr::filter(isTRUE(owns_cs2)) |> dplyr::pull(steamid)
    dplyr::bind_rows(purrr::map(keep, steam_get_inventory_730))
  }),
  tar_target(buff_weekly_path, buff_download_weekly("data/price-history-weekly.json.xz")),
  tar_target(buff_weekly, buff_read_jsonxz(buff_weekly_path)),
  tar_target(buff_subset, {
    items <- inv_all %>% dplyr::filter(!is.na(market_hash_name)) %>% dplyr::distinct(market_hash_name) %>% dplyr::pull()
    items <- head(items, BUFF_MAX_ITEMS)
    series_list <- lapply(items, function(nm) { df <- buff_series(buff_weekly, nm); df$item <- nm; df })
    dplyr::bind_rows(series_list)
  }),
  tar_target(inv_priced, {
    last_price <- buff_subset %>%
      dplyr::group_by(item) %>%
      dplyr::filter(ts == max(ts)) %>%
      dplyr::ungroup() %>%
      dplyr::select(item, last_price_cny = price_cny, last_listings = listings)
    inv_all %>% dplyr::left_join(last_price, by = c("market_hash_name" = "item"))
  }),
  # archivos de salida
  tar_target(seed_ids_csv, {
    readr::write_csv(tibble(steamid = seed_ids), "out/seed_ids.csv"); "out/seed_ids.csv"
  }, format = "file"),
  tar_target(bfs_edges_csv, {
    readr::write_csv(bfs$edges, "out/bfs_edges.csv"); "out/bfs_edges.csv"
  }, format = "file"),
  tar_target(bfs_nodes_csv, {
    readr::write_csv(tibble(steamid = bfs$nodes), "out/bfs_nodes.csv"); "out/bfs_nodes.csv"
  }, format = "file"),
  tar_target(users_cs2_csv, {
    readr::write_csv(users_cs2, "out/users_cs2.csv"); "out/users_cs2.csv"
  }, format = "file"),
  tar_target(inv_all_csv, {
    readr::write_csv(inv_all, "out/inventory_730.csv"); "out/inventory_730.csv"
  }, format = "file"),
  tar_target(buff_subset_csv, {
    readr::write_csv(buff_subset, "out/buff_weekly_subset.csv"); "out/buff_weekly_subset.csv"
  }, format = "file"),
  tar_target(inv_priced_csv, {
    readr::write_csv(inv_priced, "out/inventory_730_priced.csv"); "out/inventory_730_priced.csv"
  }, format = "file")
)
