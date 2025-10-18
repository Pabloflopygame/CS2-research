
# scripts/03_owned_games.R
source("../R/utils.R")

seed_ids <- if (file.exists("../out/seed_ids.rds")) readRDS("../out/seed_ids.rds") else character()
bfs_nodes <- if (file.exists("../out/bfs_nodes.csv")) readr::read_csv("../out/bfs_nodes.csv", show_col_types = FALSE)$steamid else character()

candidates <- unique(c(seed_ids, bfs_nodes))
message("Candidatos a revisar CS2: ", length(candidates))

users_cs2 <- has_cs2_many(candidates)
readr::write_csv(users_cs2, "../out/users_cs2.csv")
saveRDS(users_cs2, "../out/users_cs2.rds")
message("Guardado: out/users_cs2.csv y out/users_cs2.rds")
