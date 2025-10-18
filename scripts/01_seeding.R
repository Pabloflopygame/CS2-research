# scripts/01_seeding.R
source("../R/utils.R")

# Verbosidad global via env var (VERBOSE=1) o automático
VERBOSE <- as.logical(as.integer(Sys.getenv("VERBOSE", if (interactive()) "1" else "0")))
LOGFILE <- Sys.getenv("SEED_LOGFILE", "")  # e.g., SEED_LOGFILE=../logs/seeding.log

if (nzchar(LOGFILE)) {
  dir.create(dirname(LOGFILE), showWarnings = FALSE, recursive = TRUE)
  zz <- file(LOGFILE, open = "at", encoding = "UTF-8")
  sink(zz, type = "output", split = TRUE)
  sink(zz, type = "message", split = TRUE)
  on.exit({ sink(type="message"); sink(type="output"); close(zz) }, add = TRUE)
  message("== Logging en: ", normalizePath(LOGFILE, winslash = "/"))
}

groups <- readLines("../config/seed_groups.txt", warn = FALSE)
message("Grupos a procesar: ", length(groups))

# Barra de progreso (sin paquetes extra)
pb_total <- length(groups)
tick <- function(i) {
  cat(sprintf("\rProgreso: [%d/%d] %3.0f%%", i, pb_total, 100*i/pb_total))
  if (i == pb_total) cat("\n")
}

seed_ids <- character()
for (i in seq_along(groups)) {
  g <- groups[[i]]
  ids <- steam_group_members(g, delay_sec = 2, verbose = VERBOSE)
  seed_ids <- unique(c(seed_ids, ids))
  if (VERBOSE) message(sprintf("[SEED] +%d nuevos | total global: %d", length(setdiff(ids, seed_ids)), length(seed_ids)))
  tick(i)
}

message("steamIDs obtenidos (únicos): ", length(seed_ids))
readr::write_csv(tibble(steamid = seed_ids), "../out/seed_ids.csv")
saveRDS(seed_ids, "../out/seed_ids.rds")
message("Guardado en ../out/seed_ids.csv y ../out/seed_ids.rds")
