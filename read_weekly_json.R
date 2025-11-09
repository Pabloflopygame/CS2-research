# ----0) Librerías----
library(jsonlite)

# ----1) Cargar JSON diario----
setwd(".")
raw_data <- fromJSON("./Data/price-history-weekly.json")

# ----2) Extraer todas las fechas----
#  Normalizarlas a semana (por ISO, International Standards Organization, 
# empieza lunes. Mientras que R lo hace en Domingos, por lo que lo adaptamos).
dates_unix <- unlist(lapply(raw_data, function(x) x[1, ]), use.names = FALSE)
dates_full <- as.Date(as.POSIXct(dates_unix, origin = "1970-01-01", tz = "UTC"))

# Restamos 1 día, cortamos por "week" (que empieza domingo), y volvemos a sumar 1.
# De esta manera podemos hacer semanas ISO's.
week_start_all <- as.Date(cut(dates_full - 1, "week")) + 1

weeks_unique <- sort(unique(week_start_all))
# Etiquetas "YYYY-Www" con año ISO:
week_labels <- strftime(weeks_unique, format = "%G-W%V", tz = "UTC")

rm(dates_unix, dates_full, week_start_all); gc()

# ----3) Prealocar matrices arma x semana----
weapons  <- names(raw_data)
nWeapons <- length(weapons)
nWeeks   <- length(weeks_unique)

precio_mat <- matrix(NA_real_, nrow = nWeapons, ncol = nWeeks,
                     dimnames = list(weapons, week_labels))
oferta_mat <- matrix(NA_real_, nrow = nWeapons, ncol = nWeeks,
                     dimnames = list(weapons, week_labels))

# ----4) Rellenar matrices agrupando por semana----
for (w in seq_len(nWeapons)) {
  item <- raw_data[[w]]
  
  d_all <- as.Date(as.POSIXct(item[1, ], origin = "1970-01-01", tz = "UTC"))
  # Semana ISO (lunes) para cada observación
  wk_all <- as.Date(cut(d_all - 1, "week")) + 1
  
  idx_week <- match(wk_all, weeks_unique)   # índice de columna (1..nWeeks)
  precio_num <- as.numeric(item[2, ]) / 100
  oferta_num <- as.numeric(item[3, ])
  
  # La function(ix) es que nos quedamos con la última observación.
  # Esto es como tratamos si un arma tiene +1 observación en la semana,
  # que no debería por como tenemos los datos. Pero por si acaso lo añadimos.
  precio_by_wk <- tapply(seq_along(precio_num), idx_week, function(ix) {
    precio_num[ix][order(d_all[ix])][length(ix)]
  })
  oferta_by_wk <- tapply(seq_along(oferta_num), idx_week, function(ix) {
    oferta_num[ix][order(d_all[ix])][length(ix)]
  })
  
  cols_present <- as.integer(names(precio_by_wk))
  precio_mat[w, cols_present] <- as.numeric(precio_by_wk)
  oferta_mat[w, cols_present] <- as.numeric(oferta_by_wk)
}

rm(item, d_all, wk_all, idx_week, precio_num, oferta_num,
   precio_by_wk, oferta_by_wk, cols_present, w); gc()

# ----5) Construir el data.frame salida (columnas-lista por semana)----
data <- data.frame(nombre = weapons, stringsAsFactors = FALSE)

for (j in seq_len(nWeeks)) {
  col_name <- colnames(precio_mat)[j]
  col_list <- vector("list", nWeapons)
  rows_with_data <- which(!is.na(precio_mat[, j]))
  if (length(rows_with_data) > 0) {
    col_list[rows_with_data] <- mapply(
      function(o, p) list(oferta = o, precio = p),
      oferta_mat[rows_with_data, j],
      precio_mat[rows_with_data, j],
      SIMPLIFY = FALSE, USE.NAMES = FALSE
    )
  }
  data[[col_name]] <- col_list
}

rm(col_name, col_list, rows_with_data, j); gc()

# ----6) Limpieza final----
rm(raw_data, weapons, weeks_unique, week_labels, nWeapons, nWeeks,
   precio_mat, oferta_mat); gc()
detach("package:jsonlite")

# ----7) Guardar----
saveRDS(data, file = "./out/data_weekly.rds", compress = "xz")
# data <- readRDS("./out/data_weekly.rds")
