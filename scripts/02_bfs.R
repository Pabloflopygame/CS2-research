# scripts/02_bfs.R
# BFS con progreso por capa, telemetría por petición y logging opcional.
# Corrige el caso en el que #seeds > MAX_NODES añadiendo un PAD configurable.

# --- Cargar variables de entorno del proyecto ---
if (file.exists("../.Renviron")) readRenviron("../.Renviron")
if (file.exists(".Renviron"))    readRenviron(".Renviron")  # por si ejecutas desde la raíz

# --- Utilidades / API helpers ---
source("../R/utils.R")

# --- Verbosidad y logging ---
VERBOSE  <- as.logical(as.integer(Sys.getenv("VERBOSE", if (interactive()) "1" else "0")))
LOGFILE  <- Sys.getenv("BFS_LOGFILE", "")  # p.ej., ../logs/bfs.log

# --- Asegura carpetas de salida y logs ---
dir.create("../out",  showWarnings = FALSE, recursive = TRUE)
dir.create("../logs", showWarnings = FALSE, recursive = TRUE)

# --- Activa logging a fichero si se configura ---
if (nzchar(LOGFILE)) {
  zz <- file(LOGFILE, open = "at", encoding = "UTF-8")
  sink(zz, type = "output",  split = TRUE)
  sink(zz, type = "message", split = TRUE)
  on.exit({ sink(type="message"); sink(type="output"); close(zz) }, add = TRUE)
  message("== Logging en: ", normalizePath(LOGFILE, winslash = "/"))
}

# --- Parámetros (puedes sobreescribir vía variables de entorno) ---
BFS_DEPTH      <- as.integer(Sys.getenv("BFS_DEPTH", "2"))
MAX_NODES      <- as.integer(Sys.getenv("MAX_NODES", "30000"))   # tope tradicional
BFS_PAD_NODES  <- as.integer(Sys.getenv("BFS_PAD_NODES", "30000")) # añadimos margen sobre #seeds
DELAY_SEC      <- as.numeric(Sys.getenv("BFS_DELAY", "0.3"))
SLEEP_ON429    <- as.numeric(Sys.getenv("BFS_SLEEP_ON429", "45"))
MAX_ERRORS     <- as.integer(Sys.getenv("BFS_MAX_ERRORS", "120"))
CHUNK          <- as.integer(Sys.getenv("BFS_CHUNK", "0"))       # procesa solo primeros N seeds si > 0

# --- Semillas ---
seed_path <- "../out/seed_ids.rds"
if (!file.exists(seed_path)) stop("No existe ../out/seed_ids.rds; ejecuta primero 01_seeding.R")
seed_ids <- readRDS(seed_path)

if (CHUNK > 0L && length(seed_ids) > CHUNK) {
  seed_ids <- seed_ids[seq_len(CHUNK)]
  message("Usando chunk de seeds: ", length(seed_ids))
}

SEED_N <- length(seed_ids)
# Tope efectivo: como mínimo, seeds + PAD
EFFECTIVE_MAX_NODES <- max(MAX_NODES, SEED_N + BFS_PAD_NODES)

message("BFS depth = ", BFS_DEPTH,
        " | MAX_NODES (solicitado) = ", MAX_NODES,
        " | seeds = ", SEED_N,
        " | CAP efectivo = ", EFFECTIVE_MAX_NODES,
        " (PAD=", BFS_PAD_NODES, ")")

# --- Inicialización BFS ---
visited   <- new.env(parent = emptyenv())
q         <- unique(seed_ids)
edges     <- list()
layer     <- 0L
all_nodes <- unique(seed_ids)  # mantenemos seeds dentro de all_nodes

progress_bar <- function(i, total, prefix = "") {
  pct <- floor(100 * i / max(1, total))
  txt <- sprintf("\r%s[%d/%d] %3d%%", prefix, i, total, pct)
  cat(txt)
  if (i == total) cat("\r", txt, "\n", sep = "")
  flush.console()
}

err_count <- 0L
t0 <- Sys.time()

# Historial para media móvil de latencia
bfs_meta <- new.env(parent = emptyenv())
bfs_meta$dt_hist <- numeric(0)

# --- Bucle BFS ---
while (length(q) > 0 && layer < BFS_DEPTH && length(all_nodes) < EFFECTIVE_MAX_NODES) {
  layer   <- layer + 1L
  current <- q
  q       <- character()
  
  n_curr <- length(current)
  message(sprintf("Capa %d → expandiendo %d nodos...", layer, n_curr))
  
  t_layer      <- Sys.time()
  added_nodes  <- 0L
  added_edges  <- 0L
  count_200    <- 0L
  count_private<- 0L
  count_other  <- 0L
  
  for (i in seq_along(current)) {
    u <- current[[i]]
    
    # Saltar si ya visitado
    if (!is.null(visited[[u]])) {
      if (VERBOSE && i %% 50 == 0) progress_bar(i, n_curr, prefix = sprintf("Layer %d ", layer))
      next
    }
    visited[[u]] <- TRUE
    
    # Ritmo base adicional (aparte del limitador interno de utils.R si lo tienes)
    if (DELAY_SEC > 0) Sys.sleep(DELAY_SEC)
    
    # Llamada protegida a la API
    vs  <- NULL
    err <- FALSE
    t_req <- Sys.time()
    tryCatch({
      # steam_get_friends (definido en utils.R) deja meta en options(): status, elapsed, from_cache
      vs <<- steam_get_friends(u)
    }, error = function(e) {
      err <<- TRUE
      msg <- conditionMessage(e)
      message(sprintf("  [WARN] steam_get_friends(%s) falló: %s", u, msg))
      if (grepl("Reintentos agotados", msg, fixed = TRUE)) {
        message(sprintf("  [INFO] Pausando %.0fs por posible 429/timeout.", SLEEP_ON429))
        Sys.sleep(SLEEP_ON429)
      }
    })
    
    # --- Telemetría por petición (a prueba de NAs) ---
    code_val   <- getOption("cs2.last_status", NA_integer_)
    from_cache <- getOption("cs2.from_cache", NA)
    elapsed    <- getOption("cs2.elapsed", NA_real_)
    n_friends  <- if (is.null(vs)) 0L else length(vs)
    
    code_chr   <- if (is.na(code_val)) "NA" else as.character(code_val)
    cache_chr  <- if (isTRUE(from_cache)) "Y" else if (identical(from_cache, FALSE)) "N" else "NA"
    elapsed_s  <- if (is.na(elapsed)) as.numeric(difftime(Sys.time(), t_req, units = "secs")) else elapsed
    
    bfs_meta$dt_hist <- c(bfs_meta$dt_hist, elapsed_s)
    if (length(bfs_meta$dt_hist) > 10) bfs_meta$dt_hist <- tail(bfs_meta$dt_hist, 10)
    ma10 <- mean(bfs_meta$dt_hist)
    
    if (VERBOSE) {
      u_short <- substr(u, 1, 17)
      message(sprintf("[%s] #%d/%d id=%s status=%s cache=%s dt=%.2fs (MA10=%.2fs) friends=%d queue=%d",
                      format(Sys.time(), "%H:%M:%S"),
                      i, n_curr, u_short, code_chr, cache_chr,
                      elapsed_s, ma10, n_friends, length(q)))
    }
    
    # Contadores por estado
    if (!is.na(code_val)) {
      if (code_val == 200L) count_200 <- count_200 + 1L
      else if (code_val %in% c(401L, 403L)) count_private <- count_private + 1L
      else count_other <- count_other + 1L
    }
    
    if (err) {
      err_count <- err_count + 1L
      if (err_count >= MAX_ERRORS) {
        stop(sprintf("Demasiados errores consecutivos (%d). Aborto BFS.", err_count))
      }
      next
    } else {
      err_count <- 0L
    }
    
    # Actualiza aristas y cola de la siguiente capa
    if (length(vs)) {
      df <- tibble::tibble(from = u, to = vs)
      edges[[length(edges) + 1]] <- df
      added_edges <- added_edges + nrow(df)
      
      nv <- setdiff(vs, names(visited))
      q  <- unique(c(q, nv))
      
      before      <- length(all_nodes)
      all_nodes   <- unique(c(all_nodes, vs))
      added_nodes <- added_nodes + (length(all_nodes) - before)
    }
    
    if (VERBOSE && (i %% 25 == 0 || i == n_curr)) {
      progress_bar(i, n_curr, prefix = sprintf("Layer %d ", layer))
    }
    
    if (length(all_nodes) >= EFFECTIVE_MAX_NODES) {
      message("CAP de nodos alcanzado; se detiene la expansión.")
      break
    }
  }
  
  dt_layer <- as.numeric(difftime(Sys.time(), t_layer, units = "secs"))
  message(sprintf(
    "Capa %d completada en %.1fs | 200=%d  401/403=%d  otros=%d | nuevas aristas: %d | total nodos: %d",
    layer, dt_layer, count_200, count_private, count_other, added_edges, length(all_nodes)
  ))
}

# --- Resumen y guardado ---
dt_total <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
E <- if (length(edges)) dplyr::bind_rows(edges) else tibble::tibble(from = character(), to = character())

message(sprintf("BFS terminado en %.1fs | nodos: %d | aristas: %d",
                dt_total, length(all_nodes), nrow(E)))

readr::write_csv(E,                                "../out/bfs_edges.csv")
readr::write_csv(tibble::tibble(steamid = all_nodes), "../out/bfs_nodes.csv")
saveRDS(list(nodes = all_nodes, edges = E),        "../out/bfs.rds")
message("Guardado: ../out/bfs_edges.csv, ../out/bfs_nodes.csv, ../out/bfs.rds")

