# ----Librerías checkup----
paquetes <- c("jsonlite", "data.table")

# Si no tienes configurado un repertorio para instalar librerías configuramos el default.
if (is.null(getOption("repos")) || length(getOption("repos")) == 0 || is.na(getOption("repos"))) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

# Verificamos cada paquete
for (paquete in paquetes) {

  if (!requireNamespace(paquete, quietly = TRUE)) {
    cat("\nEl paquete '", paquete, "' no está instalado.\n", sep = "")
    
    repeat {
      respuesta <- tolower(trimws(readline(prompt = "¿Quieres instalarlo? [s/n]: ")))
  
      if (respuesta %in% c("s")) {
        install.packages(paquete)
  
        if (!requireNamespace(paquete, quietly = TRUE)) {
          stop(paste("La instalación de", paquete, "falló. Abortando."), call. = FALSE)
        } else {
          cat("Paquete", paquete, "instalado correctamente.\n")
        }
        break # continuamos al siguiente paquete (si no vamos al else).

      } else if (respuesta %in% c("n")) {
        cat("El usuario canceló la instalación de", paquete, ". Abortando.", sep = "")
        stop("Necesita instalar las librerías para ejecutar el programa", call. = FALSE)

      } else {
        cat("Input inválido, responde con 's' o 'n'.\n")
      }
    }

  } else {
    cat("Paquete", paquete, "ya instalado.\n")
  }
}

# Confirmación final
cat("\n----Todas las librerías requeridas están instaladas.\n")

# limpiamos memoria
rm(paquete, paquetes)

# ----Files checkup----
ficheros <- c(
  "./out/data_daily.rds",
  "./out/data_monthly.rds",
  "./out/data_raw_long.rds",
  "./out/data_weekly-2.rds",
  "./out/data_weekly.rds"
)

# Comprobar si faltan ficheros
ficheros_faltantes <- ficheros[!file.exists(ficheros)]
if (length(ficheros_faltantes) == 0) {
  cat("----Todos los ficheros requeridos existen.\n")
} else {
  cat("Faltan los siguientes ficheros:\n", paste(" -", ficheros_faltantes, collapse = "\n"), "\n", sep = "")
  
  repeat {
    input <- tolower(trimws(readline(prompt = "¿Quieres usar 'Data' files para crear los dataframes (.rd) faltantes? (esto necesita que los ficheros esten en data o saltará error) [s/n]: ")))
    if (input %in% c("s")) {
      cat("Creando ficheros faltantes...\n")
      for (fichero in ficheros_faltantes) {
        cat(" → Creando:", fichero, "\n")
        
        switch(fichero,
               "./out/data_daily.rds" = {
                 source("read_daily_json.R")
               },
               "./out/data_monthly.rds" = {
                 source("read_monthly_json.R")
               },
               "./out/data_raw_long.rds" = {
                 source("read_raw_json.R")
               },
               "./out/data_weekly-2.rds" = {
                 source("read_weekly_2_json.R")
               },
               "./out/data_weekly.rds" = {
                 source("read_weekly_json.R")
               }
              )
      }
      cat("----Todos los ficheros faltantes fueron creados.\n")
      
      rm(fichero)
      break
    
    } else if (input %in% c("n")) {
      stop("Necesita crear/añadir los .rd para continuar.", call. = FALSE)
    
    } else {
      cat("Por favor, responde con 's' o 'n'.\n")
    }
  }
}

rm(ficheros, ficheros_faltantes); gc()

# ----Basic check----
# cargamos un database básico.
data_month <- readRDS("./out/data_monthly.rds")

# ----0) Librerías----
library(tidyverse)

# ----1) Pasar de formato ancho a largo----
data_long <- data_month %>%
  pivot_longer(
    cols = -nombre,                # todas las columnas menos 'nombre'
    names_to = "mes",              # nombre de la variable (ej. "2021-07")
    values_to = "valores"          # las listas con oferta/precio
  ) %>%
  # Filtrar solo las celdas que no son NULL
  filter(!sapply(valores, is.null)) %>%
  # Extraer oferta y precio en columnas separadas
  mutate(
    oferta = sapply(valores, function(x) x$oferta),
    precio = sapply(valores, function(x) x$precio)
  ) %>%
  select(-valores)

# ----2) Convertir tipos----
data_long <- data_long %>%
  mutate(
    mes = as.Date(paste0(mes, "-01")),  # convertir "2021-07" → 2021-07-01
    oferta = as.numeric(oferta),
    precio = as.numeric(precio)
  )

# ----3) Gráfico con ggplot2----
ggplot(data_long, aes(x = mes, y = precio, color = nombre)) +
  geom_line(linewidth = 0.7, alpha = 0.8) +
  labs(
    title = "Evolución mensual del precio por arma",
    x = "Mes",
    y = "Precio",
    color = "Arma"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )
