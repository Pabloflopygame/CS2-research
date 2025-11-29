# Warning!!! 
# Puede tardar ~12 HORAS para nuestro dataset
# No hace falta borrar el CSV temporal append = False borra lo que haya.


setDTthreads(percent = 100) # recomendado tener

# Quitamos observaciones en la cual 1 arma tiene 2 o más observaciones
# en el mismo segundo. Esto es por razones técnicas y para que cuando
# saquemos las lineas que necesitemos para un dataframe no tengas que
# verificar que haya varias observaciones en el mismo segundo 
# (si las hubiera hacer un dataframe sería difícil ya que hacemos columnas
# como fechas y no pueden haber 2 columnas iguales).
# Por lo que nos quedamos con la última observación en 1 mismo segundo.

# ----0) Librerías----
library(jsonlite)

# Opcional pero MUY recomendado para escribir rápido y en streaming.
# Para no mostrar los warnings para ir más rápido, pero requiere comprobar que 
# data.table este instalado.
suppressWarnings({
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Instala data.table: install.packages('data.table')")
  }
})
library(data.table)

# ----1) Cargar JSON (misma ruta)----
setwd("./")
raw_data <- fromJSON("./Data/price-history-raw.json")

# ----2) Archivo de salida (formato largo)----
out_path <- "./out/data_raw_long.csv"
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)

# Escribir encabezado vacío una sola vez
fwrite(
  data.table(nombre = character(), datetime = as.POSIXct(character()), 
             precio = double(), oferta = double()),
  file = out_path, append = FALSE
)

# ----3) Recorrer armas y volcar a disco en streaming----
weapons <- names(raw_data)
nWeapons <- length(weapons)

for (w in seq_len(nWeapons)) {
  arma  <- weapons[w]
  item  <- raw_data[[w]]
  
  # timestamps a POSIXct (segundos)
  dt <- as.POSIXct(item[1, ], origin = "1970-01-01", tz = "UTC")
  
  # precio en unidades (estaba en centiyenes)
  precio <- as.numeric(item[2, ]) / 100
  oferta <- as.numeric(item[3, ])
  
  # Si hay múltiples lecturas en el mismo segundo para esta arma,
  # nos quedamos con la ÚLTIMA por orden cronológico.
  ord <- order(dt)
  dt_ord <- dt[ord]
  precio_ord <- precio[ord]
  oferta_ord <- oferta[ord]
  
  keep <- !duplicated(dt_ord, fromLast = TRUE)  # TRUE = primera vez desde el final => última observación
  dt_keep <- dt_ord[keep]
  precio_keep <- precio_ord[keep]
  oferta_keep <- oferta_ord[keep]
  
  # Construir bloque y escribir en append
  block <- data.table(
    nombre  = arma,
    datetime = dt_keep,
    precio   = precio_keep,
    oferta   = oferta_keep
  )
  
  fwrite(block, file = out_path, append = TRUE)
  
  # Liberar memoria de objetos grandes de este ciclo
  rm(item, dt, precio, oferta, ord, dt_ord, precio_ord, oferta_ord, 
     keep, dt_keep, precio_keep, oferta_keep, block)
  # no hacemos gc para no desperdiciar recursos.
}

# ----4) Limpieza final----
rm(raw_data, weapons, nWeapons, w, out_path, datetime, arma); gc()
detach("package:jsonlite")

# para cargar los datos
DT <- fread("./out/data_raw_long.csv",
            showProgress = TRUE,
            colClasses = list(character = "nombre",
                              # lee datetime como texto por velocidad
                              character = "datetime",
                              numeric   = c("precio", "oferta")
                              )
            )

# guardarlo en un formato más compacto y rápido
saveRDS(DT, file = "./out/data_raw_long.rds", compress = "xz")
#
# para cargarlo:
# DT <- readRDS("./out/data_raw_long.rds")

detach("package:data.table")
