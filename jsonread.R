library(jsonlite)

setwd(".")
data_list <- fromJSON("./Data/price-history-daily.json")
# genera una lista como:
# [pistola, matrix 3 x n_observaciones, rifle, matrix 3 x n_observaciones, ...]
# Cada matrix a su vez es realmente una lista (bidimencional, pero type list) como:
# [
# fecha_observación, fecha_observación, ...
# precio,            precio,            ...
# oferta,            oferta,            ...
# ]
# *La oferta es cuanta gente esta poniendo a la venta ese item en particular
# Realmente la gente puede ponerlo a la venta en diferentes cantidades, pero
# para que los datos no pesen Tb's, lo hemos simplificado a:
#
# queremos generar un dataframe datos con columnas como:
# nombre, fecha1, fecha2, fecha3, ...
# Para cada fecha queremos que haya una lista con [oferta: precio]

# sacamos los nombres
weapons <- names(data_list)

# sacanos las fechas
dates <- list()
for (i in seq_along(data_list)) {
  dates[[i]] <- data_list[[i]][1, ]  # o [[1]]
}
rm(i)
# como tenemos para [0] (del arma índice 0) --> (fecha1, fecha2, fecha3), [1] --> (...)
# tenemos que unlist it, quitar el use.name (para que no haga nada raro tratando
# de conservar la idea que estaban en sus propios vectores cambiando los nombres)
# Y ya dsp hacemos unique para quedarnos con las fechas sin repeticiones.
dates <- unique(unlist(dates, use.names = FALSE))
dates <- as.Date(as.POSIXct(dates, origin = "1970-01-01", tz = "UTC"))

# Ahora que tenemos las columnas podemos construir el dataframe y llenarlo de NA's
# por ahora.
data <- data.frame(
  nombre = weapons,
  matrix(NA, nrow = length(weapons), ncol = length(dates))
)

# realmente no hemos añadido nombres en la columna, sino que matrix le ha puesto 
# valores por defecto, ahora simplemente lo sobrescribimos.
# *Añadimos nombre simplemente para aprobechar para meter los nombres con ella.
colnames(data) <- c("nombre", as.character(dates))

# Ahora toca lo más divertido! Ir arma por arma, añadiendo en las fechas
# que tengan registros sus ofertas y valor :3

# Ayuda, no puedo hacerlo sin quemar la CPU



#---- ChatGPT magia negra ----
library(jsonlite)

# Cargar JSON
data_list <- fromJSON("./Data/price-history-daily.json")
weapons <- names(data_list)

# Fechas únicas globales
dates <- unique(unlist(lapply(data_list, function(x) x[1, ]), use.names = FALSE))
dates <- as.Date(as.POSIXct(dates, origin="1970-01-01", tz="UTC"))

# Prealocamos matrices
nW <- length(weapons); nD <- length(dates)
precio_mat <- matrix(NA_real_, nrow = nW, ncol = nD,
                     dimnames = list(weapons, as.character(dates)))
oferta_mat <- matrix(NA_real_, nrow = nW, ncol = nD,
                     dimnames = list(weapons, as.character(dates)))

# Relleno vectorizado (rápido)
for (w in seq_along(weapons)) {
  item <- data_list[[w]]
  dvec <- as.Date(as.POSIXct(item[1, ], origin="1970-01-01", tz="UTC"))
  pvec <- as.numeric(item[2, ])
  ovec <- as.numeric(item[3, ])
  idx <- match(dvec, dates)
  
  precio_mat[w, idx] <- pvec
  oferta_mat[w, idx] <- ovec
}
library(purrr)
library(tibble)

data <- tibble(
  nombre = rownames(precio_mat)
)

for (j in seq_along(dates)) {
  d <- as.character(dates[j])
  data[[d]] <- map(seq_len(nW), function(i) {
    if (is.na(precio_mat[i, j])) {
      NA
    } else {
      list(oferta = oferta_mat[i, j], precio = precio_mat[i, j])
    }
  })
}
