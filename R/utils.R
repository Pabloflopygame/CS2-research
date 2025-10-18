
# R/utils.R

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(xml2)
  library(dplyr)
  library(purrr)
  library(tibble)
  library(tidyr)
  library(stringr)
  library(igraph)
  library(lubridate)
  library(readr)
})

STEAM_API_BASE <- Sys.getenv("STEAM_API_BASE", "https://api.steampowered.com")
ua <- "R-CS2-research/1.0 (+local)"

.stop_if_no_key <- function() {
  key <- Sys.getenv("STEAM_API_KEY", unset = "")
  if (!nzchar(key)) stop("Falta STEAM_API_KEY (.Renviron o Sys.setenv).")
  invisible(key)
}

`%||%` <- function(a,b) if (is.null(a)) b else a

# === Config/Helpers ===
ua <- "R-CS2-research/1.0 (+github.com/tu-org)"
pause <- function(s=1.0) Sys.sleep(s)
chunk <- function(x, n) split(x, ceiling(seq_along(x)/n))

.stop_if_no_key <- function() {
  key <- Sys.getenv("STEAM_API_KEY", unset = "")
  if (!nzchar(key)) {
    stop("Falta STEAM_API_KEY. Ponla en .Renviron o Sys.setenv() y vuelve a ejecutar.")
  }
  invisible(key)
}

.req_perform_retry <- function(req, tries = 8, base_sleep = 1.0, max_sleep = 45) {
  for (i in seq_len(tries)) {
    t0 <- Sys.time()
    resp <- try(req_perform(req), silent = TRUE)
    dt  <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    
    # ¿llegó respuesta HTTP?
    if (!inherits(resp, "try-error")) {
      code <- resp_status(resp)
      if (code >= 200 && code < 300) return(resp)
      if (code %in% c(401, 403)) {
        message(sprintf(".req_perform_retry: HTTP %d (lista privada). No reintento.", code))
        return(resp)
      }
      # 429/5xx → backoff
      ra <- resp_headers(resp)[["retry-after"]]
      sleep <- if (!is.null(ra) && suppressWarnings(!is.na(as.numeric(ra)))) as.numeric(ra)
      else min(max_sleep, base_sleep * (2^(i-1))) * runif(1, 0.75, 1.35)
      message(sprintf(".req_perform_retry: HTTP %d (%.2fs). Reintentando en %.1fs [%d/%d]",
                      code, dt, sleep, i, tries))
      Sys.sleep(sleep)
      next
    }
    
    # error de red/timeout (sin respuesta)
    sleep <- min(max_sleep, base_sleep * (2^(i-1))) * runif(1, 0.75, 1.35)
    message(sprintf(".req_perform_retry: error de red. Reintentando en %.1fs [%d/%d]", sleep, i, tries))
    Sys.sleep(sleep)
  }
  stop("Reintentos agotados: ", req$url)
}



# === Seeding por grupos (XML) ===
steam_group_members <- function(group_url, delay_sec = 2, verbose = NULL, max_pages = Inf) {
  # verbose por defecto: TRUE si estás en RStudio/interactive
  if (is.null(verbose)) verbose <- interactive()
  
  base <- sub("/?$","", group_url)
  if (!grepl("memberslistxml", base)) base <- paste0(base, "/memberslistxml/?xml=1")
  
  out <- character()
  page <- 1L
  total_new <- 0L
  
  if (verbose) message("[SEED] Grupo: ", group_url)
  
  repeat {
    if (page > max_pages) {
      if (verbose) message("  ↳ max_pages alcanzado (", max_pages, "), paro.")
      break
    }
    
    url <- if (grepl("\\?xml=1$", base)) paste0(base, "&p=", page) else paste0(base, "&p=", page)
    
    if (verbose) message(sprintf("  · Página %d → GET %s", page, url))
    pause(delay_sec)
    
    t0 <- Sys.time()
    doc <- try(read_xml(url, options = c("RECOVER","NOERROR","NOWARNING")), silent = TRUE)
    dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    
    if (inherits(doc, "try-error")) {
      if (verbose) message(sprintf("    ! Error leyendo XML (%.2fs). Sigo a la siguiente página.", dt))
      break
    }
    
    ids <- xml_find_all(doc, ".//steamID64") |> xml_text()
    n_ids <- length(ids)
    
    if (n_ids == 0) {
      if (verbose) message(sprintf("    · Sin IDs en la página (%.2fs). Fin del grupo.", dt))
      break
    }
    
    before <- length(out)
    out <- unique(c(out, ids))
    added <- length(out) - before
    total_new <- total_new + added
    
    if (verbose) {
      message(sprintf("    · IDs en página: %d | nuevos añadidos: %d | acumulado grupo: %d (%.2fs)",
                      n_ids, added, length(out), dt))
    }
    
    next_link <- xml_find_first(doc, ".//nextPageLink") |> xml_text()
    if (is.na(next_link) || !nzchar(next_link)) {
      if (verbose) message("  ↳ No hay nextPageLink. Fin del grupo.")
      break
    }
    
    page <- page + 1L
  }
  
  if (verbose) message("[SEED] Terminado: ", group_url, " | total únicos: ", length(out))
  out
}

# Limitador por minuto: loggea cuando duerme
.rate_limit <- local({
  last <- numeric(0)
  function(max_per_min = as.integer(Sys.getenv("BFS_CALLS_PER_MIN", "20")),
           verbose = as.logical(as.integer(Sys.getenv("VERBOSE", "0")))) {
    now <- unclass(Sys.time())
    last <<- last[(now - last) < 60]
    if (length(last) >= max_per_min) {
      sleep <- 60 - (now - min(last))
      if (sleep > 0) {
        if (verbose) message(sprintf("[RATE] durmiendo %.1fs (ventana 60s: %d/%d)",
                                     sleep, length(last), max_per_min))
        Sys.sleep(sleep)
      }
      now <- unclass(Sys.time())
      last <<- last[(now - last) < 60]
    }
    last <<- c(last, unclass(Sys.time()))
  }
})

# === Amigos: ISteamUser/GetFriendList + summaries ===
steam_get_friends <- local({
  cache_dir <- Sys.getenv("FRIENDS_CACHE_DIR", "data/friends_cache")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  
  function(steamid) {
    key <- Sys.getenv("STEAM_API_KEY", "")
    if (!nzchar(key)) stop("Falta STEAM_API_KEY (.Renviron o Sys.setenv).")
    
    # --- cache rápida ---
    fcache <- file.path(cache_dir, paste0(steamid, ".json"))
    if (file.exists(fcache)) {
      dat <- try(jsonlite::fromJSON(fcache, simplifyVector = TRUE), silent = TRUE)
      if (!inherits(dat, "try-error")) {
        xs <- try(dat$friendslist$friends$steamid, silent = TRUE) %||% character()
        # Meta para el logger del BFS:
        options(cs2.last_status = 200L, cs2.from_cache = TRUE, cs2.elapsed = 0)
        return(xs)
      }
    }
    
    # --- limitador ---
    .rate_limit(verbose = as.logical(as.integer(Sys.getenv("VERBOSE", "0"))))
    
    # --- request ---
    url <- paste0(STEAM_API_BASE, "/ISteamUser/GetFriendList/v1/")
    req <- request(url) |>
      req_user_agent(ua) |>
      req_timeout(20) |>
      req_error(is_error = function(resp) FALSE) |>
      req_url_query(key = key, steamid = steamid, relationship = "friend")
    
    t0   <- Sys.time()
    resp <- .req_perform_retry(req, tries = 6, base_sleep = 1.5)
    dt   <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    code <- resp_status(resp)
    
    # Meta accesible desde el BFS
    options(cs2.last_status = code, cs2.from_cache = FALSE, cs2.elapsed = dt)
    
    if (code %in% c(401, 403)) return(character())  # lista privada
    
    dat <- resp_body_json(resp, simplifyVector = TRUE)
    
    # cache crudo
    try(writeLines(jsonlite::toJSON(dat, auto_unbox = TRUE), fcache, useBytes = TRUE), silent = TRUE)
    
    try(dat$friendslist$friends$steamid, silent = TRUE) %||% character()
  }
})


steam_get_playersummaries <- function(steamids) {
  key <- .stop_if_no_key()
  url <- "https://partner.steam-api.com/ISteamUser/GetPlayerSummaries/v2/"
  chunks <- chunk(steamids, 100)
  res <- map_dfr(chunks, function(ids) {
    req <- request(url) |>
      req_user_agent(ua) |>
      req_url_query(key = key, steamids = paste(ids, collapse=","))
    resp <- .req_perform_retry(req)
    dat <- resp_body_json(resp, simplifyVector = TRUE)
    as_tibble(dat$response$players)
  })
  res
}

crawl_friends_bfs <- function(seed_ids, depth = 1, max_nodes = 50000, delay_sec = 0.5) {
  visited <- new.env(parent = emptyenv())
  q <- unique(seed_ids)
  edges <- list()
  layer <- 0L
  all_nodes <- unique(seed_ids)
  while (length(q) > 0 && layer < depth && length(all_nodes) < max_nodes) {
    layer <- layer + 1L
    next_q <- character()
    for (u in q) {
      if (!is.null(visited[[u]])) next
      visited[[u]] <- TRUE
      pause(delay_sec)
      vs <- steam_get_friends(u)
      if (length(vs)) {
        edges[[length(edges)+1]] <- tibble(from = u, to = vs)
        nv <- setdiff(vs, names(visited))
        next_q <- unique(c(next_q, nv))
        all_nodes <- unique(c(all_nodes, vs))
      }
      if (length(all_nodes) >= max_nodes) break
    }
    q <- next_q
  }
  E <- if (length(edges)) bind_rows(edges) else tibble(from=character(), to=character())
  list(nodes = all_nodes, edges = E)
}

# === CS2 (730): IPlayerService/GetOwnedGames ===
has_cs2 <- function(steamid) {
  key <- .stop_if_no_key()
  url <- "https://partner.steam-api.com/IPlayerService/GetOwnedGames/v1/"
  req <- request(url) |>
    req_user_agent(ua) |>
    req_url_query(
      key = key,
      steamid = steamid,
      include_appinfo = 1,
      `appids_filter[0]` = 730
    )
  resp <- .req_perform_retry(req)
  if (resp_status(resp) == 401) return(tibble(steamid=steamid, owns_cs2=NA, playtime_mins=NA))
  dat <- resp_body_json(resp, simplifyVector = TRUE)
  games <- dat$response$games
  if (is.null(games) || !length(games)) {
    tibble(steamid=steamid, owns_cs2=FALSE, playtime_mins=0L)
  } else {
    tibble(
      steamid = steamid,
      owns_cs2 = TRUE,
      playtime_mins = games$playtime_forever %||% NA_integer_
    )
  }
}

has_cs2_many <- function(steamids) {
  bind_rows(lapply(steamids, has_cs2))
}

# === Inventario 730/2 (comunidad) ===
steam_get_inventory_730 <- function(steamid, delay_sec=1.2) {
  base <- sprintf("https://steamcommunity.com/inventory/%s/730/2", steamid)
  params <- list(l="english", count=5000)
  pause(delay_sec)
  req <- request(base) |> req_user_agent(ua) |> req_url_query(!!!params)
  resp <- .req_perform_retry(req)
  dat <- resp_body_json(resp, simplifyVector = TRUE)
  if (isFALSE(dat$success)) {
    return(tibble(steamid=steamid, classid=character(), instanceid=character(), name=character(),
                  market_hash_name=character(), tradable=integer(), marketable=integer(), type=character()))
  }
  assets <- as_tibble(dat$assets)
  desc   <- as_tibble(dat$descriptions)
  if (nrow(assets) == 0 || nrow(desc) == 0) {
    return(tibble(steamid=steamid)[0,])
  }
  assets <- assets |> transmute(classid, instanceid, assetid)
  inv <- assets |>
    left_join(desc |> transmute(classid, instanceid, name, market_hash_name, type, tradable, marketable),
              by=c("classid","instanceid")) |>
    mutate(steamid = steamid, .before = 1)
  inv
}

# === Buff.163 price history helpers ===
buff_download_weekly <- function(dest = "data/price-history-weekly.json.xz") {
  url <- "https://raw.githubusercontent.com/atalantus/buff-price-history-archive/main/price-history-weekly.json.xz"
  if (!file.exists(dest)) {
    download.file(url, destfile = dest, mode = "wb", quiet = TRUE)
  }
  dest
}

buff_read_jsonxz <- function(path) {
  con <- xzfile(path, open = "rb")
  on.exit(close(con), add = TRUE)
  txt <- readChar(con, nchars = 1e9, useBytes = TRUE)
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

buff_series <- function(buff_list, market_name) {
  x <- buff_list[[market_name]]
  if (is.null(x)) return(tibble())
  tibble(
    ts = as_datetime(unlist(x[[1]])),
    price_cny = as.numeric(unlist(x[[2]]))/100,
    listings  = as.integer(unlist(x[[3]]))
  )
}
