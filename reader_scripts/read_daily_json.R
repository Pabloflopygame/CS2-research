# ----0) Cargamos librerías----
library(jsonlite)

# ----1) Cargar JSON----
setwd("./")
raw_data <- fromJSON("./Data/price-history-daily.json")

# ----2) Extraer columnas----
# usando lapply (que es muy rápida comparado con un for loop) sacamos todas
# las fechas de todos los items. Después lo pasamos a vector usando unlist,
# pero como cada fecha es un vector de fechas para cada item tenemos que decirle
# que no use "use.names" ya que con el trata de añadirle un (a1, a2, ...), (b1, b2, ...), ...
# a cada fecha para que se sepa a que subconjunto pertenecía, pero no lo queremos,
# por lo que lo quitamos.
dates_unix <- unlist(lapply(raw_data, function(x) x[1, ]), use.names = FALSE)

# Admeás hicimos unlist para que ahora podamos trabajar como vector que es como
# muchas funciones como sort (para ordenar) y as.Date piden.
dates <- as.Date(as.POSIXct(dates_unix, origin = "1970-01-01", tz = "UTC"))
dates <- sort(unique(dates))

# limpiamos la memoria
# *gc llama al garbage colector. No es necesario llamarlo ya que es automático, 
# pero no hace daño llamarlo cuando haces removes.
rm(dates_unix); gc()

# ----3) Prealocar los datos en matrices----
# sacamos todos los nombres de las armas
weapons <- names(raw_data)

# sacamos las dimensiones de las matrices.
nWeapons <- length(weapons)
nDates <- length(dates)

# Usamos matrices densas (double) con NA_real_ para huecos -> rápidas y compactas.
# Necesitamos usarlas porque con 2 millones de observaciones for loops es muy lento.
precio_mat <- matrix(NA_real_, nrow = nWeapons, ncol = nDates,
                     dimnames = list(weapons, as.character(dates)))
oferta_mat <- matrix(NA_real_, nrow = nWeapons, ncol = nDates,
                     dimnames = list(weapons, as.character(dates)))

# ----4) Rellenamos las matrices para cada arma----
# Convertimos sus fechas a índices de columna con match y asignamos de golpe.
for (w in seq_len(nWeapons)) {
  # sacamos el primer item
  item <- raw_data[[w]]
  # convertimos las fechas de Unix a date.
  dvec <- as.Date(as.POSIXct(item[1, ], origin = "1970-01-01", tz = "UTC"))
  idx  <- match(dvec, dates)
  
  # asignamos los valores a las matrices
  # dividimos entre /100 el precio porque esta en "centiyenes"
  precio_mat[w, idx] <- as.numeric(item[2, ]) / 100
  oferta_mat[w, idx] <- as.numeric(item[3, ])
}

# limpiamos la memoria
rm(item, dvec, idx, w); gc()

# ----5) Construimos el dataframe----
#   - Primera columna "nombre": nombre del arma
#   - Una columna por fecha registrada: cada celda = list(oferta = ..., precio = ...) 
#     para ese momento para esa arma
#   - Si no hay datos para esa fecha/arma -> dejamos NULL (no fue registrado).

# creamos el dataframe, por ahora solo incluimos la columna nombre
data <- data.frame(nombre = weapons, stringsAsFactors = FALSE)

# Creamos las columnas por fecha como columnas-lista (similar a como hicimos nombre = weapon) 
# y rellenamos solo donde hay dato.
for (index in seq_len(nDates)) {
  # cogemos el nombre de la columna en la matrix (es un date a string).
  # *recordamos que la matrix tiene las fechas ordenadas de menor a mayor.
  col_name <- colnames(precio_mat)[index]
  
  # Prealocar columna-lista con NULL (más rápido y ligero que listas por defecto)
  col_list <- vector("list", nWeapons)
  # Filas que tienen precio (también habrá oferta en esas mismas posiciones)
  # *Aquí estamos mirando la matrix, esta si tiene NA, col_list tiene NULL's.
  rows_with_data <- which(!is.na(precio_mat[, index]))
  
  # solo si tiene datos hacemos llamamos a mapply.
  if (length(rows_with_data) > 0) {
    # Construir listas [oferta, precio] solo para posiciones con dato.
    # mapply devuelve una lista de listas (y tenemos que decirle que no
    # intente simplificar o hacer cosas raras renombrando).
    col_list[rows_with_data] <- mapply(
      function(o, p) list(oferta = o, precio = p),
      oferta_mat[rows_with_data, index],
      precio_mat[rows_with_data, index],
      SIMPLIFY = FALSE, USE.NAMES = FALSE
    )
  }
  # Añadimos la columna al data.frame
  data[[col_name]] <- col_list
}

# limpiamos la memoria
rm(col_name, col_list, rows_with_data, index); gc()

# ----6) Limpieza de final de memoria----
rm(raw_data, weapons, dates, nWeapons, nDates, precio_mat, oferta_mat); gc()
# Evidentemente lo quitamos para evitar conflictos con otras librerías
# de cara al futuro.
detach("package:jsonlite")

# ----7) Guardar----
# si lo quieres guardar en out (puede tardar un poco):
saveRDS(data, file = "./out/data_daily.rds", compress = "xz")
# para volver a abrir:
# data <- readRDS("./out/data_daily.rds")
