# ----Cargamos las librerías----
library("xml2")
library("dplyr")
library("httr2")
library("jsonlite")

# ----Comprobamos los ficheros y leemos los datos----

# comprobamos que existen los ficheros
if (!file.exists(file.path("input", "seed_groups.txt"))) {
  stop("[X] El archivo seed_groups.txt en input no existe.")
}
if (!file.exists(file.path("input", "seed_users.txt"))) {
  stop("[X] El archivo seed_users.txt en input no existe.")
}

# No podemos comprobar el contenido de los ficheros, por lo que debemos
# confiar que el usuario los ponga bien.

# leemos los grupos:
links <- readLines("./input/seed_groups.txt")

# leemos las seeds:
users <- readLines("./input/seed_users.txt")

message("[✓] Lectura correcta de groups y users")

# ----Buscamos los usuarios para cada grupo----
if (length(links) > 0) {
  message("🛈 Empezando a buscar grupos")
  source("./scripts/Steam_API.r")
  seeders <- get_multiple_groups(links)
} else {
  message("🛈 No hay links en seed_groups")
}

# añadimos los usuarios del seed_users con campo grupo "User added"
seeders <- seeders |> 
            add_row(
            steam_id = users,
            grupo = rep("User added", length(users)))

# Lo guardamos en el seed_raw.csv
write.csv(seeders, file.path(out_dir, "seed_raw.csv"))

# ----Cesgamos usuarios privados----
source("./scripts/Steam_API.r")
public_seeders <- data.frame()

for (i in seq_len(nrow(seeders))) {
  steamid <- seeders$steam_id[i]

  cat(sprintf("[%d/%d] Probando %s...\n",
              i, nrow(seeders), steamid))

  is_pub <- is_steam_user_public(steamid)

  cat(sprintf(" -> Resultado: %s\n", ifelse(is_pub, "PUBLICO ✅", "PRIVADO ❌")))

  if (is_pub) {
    public_seeders <- rbind(public_seeders, seeders[i, ])
  }

  Sys.sleep(0.25)  # evitar rate limit
}


# ----Descargamos las librerías----
# las descargamos para evitar que otras librerías de otros 
# scripts no se sobrescriban entre ellas (más legible).
detach("package:xml2")
detach("package:dplyr")
detach("package:httr2")
detach("package_jsonlite")