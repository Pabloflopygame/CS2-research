
# scripts/04_inventory.R
source("../R/utils.R")

users_path <- "../out/users_cs2.csv"
if (!file.exists(users_path)) stop("No existe out/users_cs2.csv; ejecuta 03_owned_games.R")

users_cs2 <- readr::read_csv(users_path, show_col_types = FALSE)
keep <- users_cs2 |> dplyr::filter(isTRUE(owns_cs2)) |> dplyr::pull(steamid)
message("Usuarios con CS2 detectado: ", length(keep))

inv_list <- purrr::map(keep, steam_get_inventory_730)
inv_all  <- dplyr::bind_rows(inv_list)

readr::write_csv(inv_all, "../out/inventory_730.csv")
saveRDS(inv_all, "../out/inventory_730.rds")
message("Guardado: out/inventory_730.csv y out/inventory_730.rds")
