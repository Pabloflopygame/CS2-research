# ----0) Cargamos librerías----
library(jsonlite)

# ----1) Cargar JSON----
setwd(".")
raw_data <- fromJSON("./Data/price-history-monthly.json")

# ----2) Extraer columnas----
dates_unix <- unlist(lapply(raw_data, function(x) x[1, ]), use.names = FALSE)
dates <- as.Date(as.POSIXct(dates_unix, origin = "1970-01-01", tz = "UTC"))

# Manualmente revise las ~40 columnas (usando el código de daily) para verificar
# que en cada més solo haya 1 día (tal como confirmaba el github).

# Colapsar a mes y usar etiquetas "YYYY-MM"
dates <- as.Date(cut(dates, "month"))
dates <- sort(unique(dates))
col_labels <- format(dates, "%Y-%m")

rm(dates_unix); gc()

# ----3) Prealocar los datos en matrices----
weapons <- names(raw_data)
nWeapons <- length(weapons)
nDates <- length(dates)

precio_mat <- matrix(NA_real_, nrow = nWeapons, ncol = nDates,
                     dimnames = list(weapons, col_labels))
oferta_mat <- matrix(NA_real_, nrow = nWeapons, ncol = nDates,
                     dimnames = list(weapons, col_labels))

# ----4) Rellenamos las matrices para cada arma----
for (w in seq_len(nWeapons)) {
  item <- raw_data[[w]]
  dvec <- as.Date(as.POSIXct(item[1, ], origin = "1970-01-01", tz = "UTC"))
  
  # Colapsar a mes para el match
  dvec <- as.Date(cut(dvec, "month"))
  idx  <- match(dvec, dates)
  
  precio_mat[w, idx] <- as.numeric(item[2, ]) / 100
  oferta_mat[w, idx] <- as.numeric(item[3, ])
}

rm(item, dvec, idx, w); gc()

# ----5) Construimos el dataframe----
data <- data.frame(nombre = weapons, stringsAsFactors = FALSE)

for (index in seq_len(nDates)) {
  col_name <- colnames(precio_mat)[index]
  col_list <- vector("list", nWeapons)
  rows_with_data <- which(!is.na(precio_mat[, index]))
  
  if (length(rows_with_data) > 0) {
    col_list[rows_with_data] <- mapply(function(o, p) list(oferta = o, precio = p),
                                        oferta_mat[rows_with_data, index],
                                        precio_mat[rows_with_data, index],
                                        SIMPLIFY = FALSE, USE.NAMES = FALSE
                                      )
  }
  data[[col_name]] <- col_list
}

rm(col_name, col_list, col_labels, rows_with_data, index); gc()

# ----6) Limpieza de final de memoria----
rm(raw_data, weapons, dates, nWeapons, nDates, precio_mat, oferta_mat); gc()
detach("package:jsonlite")

# ----7) Guardar----
# si lo quieres guardar en out (puede tardar un poco):
saveRDS(data, file = "./out/data_monthly.rds", compress = "xz")
# para volver a abrir:
# data <- readRDS("./out/data_monthly.rds")