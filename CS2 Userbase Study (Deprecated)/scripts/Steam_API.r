# ---- seed script ----
# extrae ID's de grupos de steam y lo guarda como un dataframe "user-group"
get_multiple_groups <- function(group_urls) {
  # extrae ID's de un grupo de steam y lo añade a un dataframe user-group
  get_group_members <- function(group_url) {
    # Avisando al usuario que estamos ejecutando:
    message(paste("🛈 Buscando ID's para el grupo: ", group_url))
    
    # Extraemos nombre del grupo
    group_name <- sub(".*/groups/([^/]+)/.*", "\\1", group_url)
    # creamos la estructura para guardar los datos.
    all_ids <- c()
    page <- 1
    
    # Steam retorna páginas de 1000 en 1000 por lo que este bucle
    # busca todas las páginas hasta que retorne 0.
    repeat {
      message(paste("🛈 Página actual: ", page))
      # hay que cambiar un poco la url para que steam nos retorne el Json.
      url <- paste0(group_url, "/memberslistxml/?xml=1&p=", page)
      
      xml <- tryCatch(suppressWarnings(read_xml(url)),
                      error = function(e) {
                                message("[!] Error (probablemente 429 / rate limit). 
                                        Esperando 5 minutos antes de reintentar...")
                                Sys.sleep(300)  # 5 minutos
                                read_xml(url)   # segundo intento
                              })
      
      ids <- xml_find_all(xml, "//steamID64") |> xml_text()
      
      # Si no retornan más ID's paramos, en caso contrario pedimos más.
      if (length(ids) == 0) {
        break
      } else {
        all_ids <- c(all_ids, ids)
        page <- page + 1
      }
      
      # Para evitar ser rate limited tenemos que esperar un poco
      # entre llamada y llamada, al final del día esto es steam web-endpoint
      # Por lo que esta más limited que la API de la app.
      Sys.sleep(10)
    }
    
    # Ahora que tenemos todos los datos los añadimos al dataframe.
    tibble(
      steam_id = all_ids,
      grupo = group_name
    )
  }
  
  # llamamos la función para cada grupo y lo juntamos en un único database.
  bind_rows(lapply(group_urls, get_group_members))
}

# comprueba si tiene amigos en públicos
.check_friends_public <- function(steamid) {
  key <- Sys.getenv("STEAM_API_KEY", unset = NA)
  if (is.na(key) || key == "") stop("Falta STEAM_API_KEY en el entorno.")
  url <- "https://api.steampowered.com/ISteamUser/GetFriendList/v1/"
  req <- request(url) |>
    req_url_query(key = key, steamid = steamid, relationship = "friend") |>
    req_user_agent("steam-privacy-check/1.0") |>
    req_error(is_error = function(resp) FALSE)  # no lances excepción en 4xx/5xx
  
  resp <- req_perform(req)
  status <- resp_status(resp)
  
  if (status == 200) {
    # Si es pública, devuelve JSON con "friendslist" (puede estar vacío, y sigue siendo público)
    return(TRUE)
  } else if (status %in% c(401, 403)) {
    return(FALSE)  # perfil/friends privados (o clave sin permiso)
  } else {
    return(FALSE)  # otros errores -> tratamos como no público
  }
}

# comprueba si tiene la librería en públicos
.check_games_public <- function(steamid) {
  key <- Sys.getenv("STEAM_API_KEY", unset = NA)
  if (is.na(key) || key == "") stop("Falta STEAM_API_KEY en el entorno.")
  url <- "https://api.steampowered.com/IPlayerService/GetOwnedGames/v1/"
  req <- request(url) |>
    req_url_query(key = key, steamid = steamid, include_appinfo = 0, include_played_free_games = 1) |>
    req_user_agent("steam-privacy-check/1.0") |>
    req_error(is_error = function(resp) FALSE)
  
  resp <- req_perform(req)
  status <- resp_status(resp)
  
  if (status == 200) {
    js <- resp_body_json(resp, simplifyVector = TRUE)
    # Si los "game details" son públicos, suele venir "response" con game_count (0 o más).
    return(!is.null(js$response))
  } else if (status %in% c(401, 403)) {
    return(FALSE)  # detalles de juegos/horas privados
  } else {
    return(FALSE)
  }
}

# Failing
# comprueba si tiene el inventario en público
.check_inventory_public <- function(steamid) {
  # Buscamos si tiene un CSGO "total_inventory_count" > 0.
  url <- sprintf("https://steamcommunity.com/inventory/%s/730/2", steamid)
  req <- request(url) |>
    req_user_agent("steam-privacy-check/1.0") |>
    req_error(is_error = function(resp) FALSE)
  resp <- req_perform(req)
  status <- resp_status(resp)
  
  if (status == 200) {
    # Si público, devuelve JSON (aunque no haya items) con campos como total_inventory_count o success
    js_txt <- resp_body_string(resp)
    # Aceptamos como público si el JSON tiene "success" o "total_inventory_count"
    has_success <- grepl('"success"', js_txt, fixed = TRUE)
    has_total   <- grepl('"total_inventory_count"', js_txt, fixed = TRUE)
    return(has_success || has_total)
  } else if (status %in% c(401, 403)) {
    return(FALSE)  # inventario privado
  } else {
    return(FALSE)
  }
}

# comprueba si tiene amigos/librería/inventario en públicos
is_steam_user_public <- function(steamid) {
  stopifnot(is.character(steamid), length(steamid) == 1)
  
  friends_ok <- .check_friends_public(steamid)
  games_ok   <- .check_games_public(steamid)
  inv_ok     <- .check_inventory_public(steamid)
  
  # Devuelve TRUE solo si las tres áreas son públicas
  all(c(friends_ok, games_ok, inv_ok))
}