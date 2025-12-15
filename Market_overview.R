# ----Readme----
# Recuerda que tienes que ejecutar el main para asegurarte que tengas
# todos los datos en orden.

# ----Librerías + Datos----
library(tidyverse)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(lubridate)
data_month <- readRDS("./out/data_monthly.rds")

# ----Min y Max del dataframe----

df_long <- data_month %>%
  pivot_longer(cols = -nombre,
               names_to = "fecha",
               values_to = "valor"
               ) %>% 
  mutate(precio = map_dbl(valor, 
                          function(x) {
                            if (is.null(x)) return(NA_real_)
                            as.numeric(x["precio"])
                            }
                          )
         )
# ejemplo de como de útil el %>% operator es :D.


filter(df_long, precio == min(precio, na.rm = TRUE))
filter(df_long, precio == max(precio, na.rm = TRUE))
# Tenemos un rango de 0 a 3_499_990... Vamos a tener que hacer unos 
# histogramas a ver su distribución de valores.

# recordamos que estamos desde el pov mensual por lo que esto es una mediana
# de cada mes:
# el database creator hizo:
# aggregate = (si NO hay ningún valor > 0) → 0
# si hay valores > 0 → mediana de esos valores > 0
# Tomado desde los valores raw que pueden observar en multiples horas al día.
#
# Por lo que no es el max/min absoluto de todos los datos observados, pero
# si una guía de que podemos esperar de forma general.
#
# Además estos 0's podrían ser porque no se midio nada durante ese mes.
# tendríamos que mirar los datos raw para confirmar.
# Pero la conclusión se mantiene, el rango de valores que podemos esperar
# es de [0, millones].
#
# También puede ser porque no exite demanda, nadie quere venderlos.

df_long_filtered <- data_month %>%
  pivot_longer(
    cols = -nombre,
    names_to = "fecha",
    values_to = "valor"
  ) %>% 
  mutate(
    precio = map_dbl(valor, function(x) {
      if (is.null(x)) return(NA_real_)
      as.numeric(x["precio"])
    }),
    oferta = map_dbl(valor, function(x) {
      if (is.null(x)) return(NA_real_)
      as.numeric(x["oferta"])
    })
  )

filter(df_long_filtered, oferta > 0, precio == min(precio, na.rm = TRUE))
filter(df_long_filtered, oferta > 0, precio == max(precio, na.rm = TRUE))

df_long_filtered %>% 
  filter(oferta > 0) %>% 
  slice_min(precio, n = Inf)

## ----Evolución del mercado----
mercado_media <- df_long %>%
  group_by(fecha) %>%
  summarise(media_precio = mean(precio, 
                                na.rm = TRUE)
            ) %>%
  arrange(fecha)

ggplot(mercado_media, aes(x = as.numeric(factor(fecha)), y = media_precio)) +
  geom_col(aes(x = as.numeric(factor(fecha))), fill = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2, color = "darkgreen") +
  scale_x_continuous(
    breaks = seq_along(mercado_media$fecha),
    labels = mercado_media$fecha
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Evolución del precio medio menusal del mercado",
    x = "Mes",
    y = "Precio medio (CN¥)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# btw en regresión lineal es la recta que mejor se ajuste a los datos
# es esperable que no sea perfecta, es la que mejor se adapta a ellos.
# de ahí que no empieze clavado en 600'ish o termine perfecto o atraviese barras.

# Pendiente de la columna (crecimiento de media por mes)
mercado_media$mes_num <- seq_len(nrow(mercado_media))
modelo <- lm(media_precio ~ mes_num, data = mercado_media)

round(coef(modelo)[["mes_num"]], 2)
# El mercado suele subir 44(CN¥) por mes 
# ~50 centimos de euro a día de hoy.

items_available <- df_long %>% 
  group_by(fecha) %>% 
  summarise(nas=sum(is.na(precio)))

ggplot(items_available) + 
  geom_col(aes(fecha, nas), fill = "steelblue") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Número de items con precio NA según el mes",
    x = "Mes",
    y = "NAs"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df_long %>% filter(!is.na(precio)), aes(x = fecha, y = precio)) +
  geom_boxplot(fill = "skyblue") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Distribución mensual del precio de las skins (boxplot)",
    x = "Mes",
    y = "Precio (CN¥)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
  # Esto es inesperado, hay MUCHOS outliers.

df_sin_outliers <- df_long %>%
  group_by(fecha) %>%
  filter({
    Q1  <- quantile(precio, 0.25, na.rm = TRUE)
    Q3  <- quantile(precio, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower <- Q1 - 1.5 * IQR
    upper <- Q3 + 1.5 * IQR
    precio >= lower & precio <= upper
  }) %>%
  ungroup()

ggplot(df_sin_outliers, aes(x = fecha, y = precio)) +
  geom_boxplot(fill = "skyblue") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Distribución mensual del precio de las skins (outliers eliminados)",
    x = "Mes",
    y = "Precio (CN¥)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# incluso sin los extremos no esperados sigue siendo muy denso

ggplot(df_sin_outliers %>% filter(precio > 0), aes(x = fecha, y = precio)) +
  geom_boxplot(fill = "skyblue") +
  scale_y_log10() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Precio mensual de las skins (outliers eliminados + escala log)",
    x = "Mes",
    y = "Precio (CN¥)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# aquí tenemos ahora que filtrar por 0's porque log10(0) es indefinido.
# Pero con una escala logaritmica podemos entender más el gráfico.

ggplot(df_long %>% filter(!is.na(precio), precio > 0), aes(x = fecha, y = precio)) +
  geom_boxplot(fill = "skyblue") +
  scale_y_log10() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Precio mensual de las skins (escala log)",
    x = "Mes",
    y = "Precio (CN¥)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Re-añadimos los extremos y podemos ver ahora algo más comprensivo.
# No hay extremos inferiores porque el mínimo valor que puede tener es 0.01
# mientras que el extremo inferior empieza en ~0.001. Por lo que no existen.
#
# ¿Estos extremos son siempre los mismos? Y si no, ¿que les paso? 
# ¿Fué un pico puntual, una burbuja, manipulación de mercado?
# ¿El crecimiento de cada skin es el que esperaríamos con el crecimiento del
# mercado? y de esa forma explicando la escala logarítmica. 
# ¿Porque nos hace falta, la respuesta está enlazada con una propiedad 
# interesante de las observaciones?

# ----Métricas del mercado----
stats_por_arma <- df_long %>%
  group_by(nombre) %>%
  summarise(
    media = mean(precio, na.rm = TRUE),
    sd = sd(precio, na.rm = TRUE),
    varianza = var(precio, na.rm = TRUE),
    CV = ifelse(media == 0, NA, sd / media)
  )

ggplot(stats_por_arma %>% filter(!is.na(varianza)), aes(x = varianza)) +
  geom_histogram(bins = 60, fill = "steelblue") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Distribución de la varianza del precio por skin",
    x = "Varianza",
    y = "Frecuencia"
  )
# Esto es algo inutil... Pero bueno, sabemos que esta entre 0 y ~1000 el 99%
# El otro 1% esta entre 1000 y 3*10**11 -_-
# hacer 30.000 bins no va a ayudar así que haremos escala logaritmica.

ggplot(stats_por_arma %>% filter(!is.na(varianza), varianza > 0), aes(x = varianza)) +
  geom_histogram(bins = 60, fill = "steelblue") +
  scale_x_log10() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Distribución de la varianza por skin (escala log)",
    x = "Varianza (log10)",
    y = "Frecuencia"
  )

# Este ayuda bastante más podemos ver que un buen trozo ~30% tiene una  
# varianza de casi 0. Otro trozo de ~40% tiene entre 0 y 10_000.
# El restante tiene entre 10_000 hasta valores tan absurdos como 10**12

ggplot(stats_por_arma %>% filter(!is.na(CV)), aes(x = CV)) +
  geom_histogram(bins = 60, fill = "darkorange") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Distribución del coeficiente de variación (CV) por skin",
    x = "Coeficiente de variación",
    y = "Frecuencia"
  )

# Este por si solo ya casi dice toda la historia. ~99% de los datos están 
# entre 0 y 1. El restante es una cola que van desde 1 hasta 6

ggplot(stats_por_arma %>% filter(!is.na(CV), CV > 0), aes(x = CV)) +
  geom_histogram(bins = 60, fill = "darkorange") +
  scale_x_log10() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Distribución del CV por skin (escala log)",
    x = "Coeficiente de variación (log10)",
    y = "Frecuencia"
  )
# Este ya si acalara incluso más, nos dice con más precisión que el centro
# está con 0.5 de coeficiente de variación.
# Como dato curioso inesperado, hay datos extremos que tienden múcho a tener
# una variación de ~0.005...


df_long_filtered %>% group_by(fecha) %>% 
  summarise(total = sum(oferta, na.rm=T)) %>%
  ggplot() + geom_col(aes(fecha, total)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


q <- quantile(df_long_filtered$precio, 0.75, na.rm=T) + IQR(df_long_filtered$precio, na.rm=T) * 1.5
outliers <- df_long_filtered[df_long_filtered$precio >= q]

# ----cleanup----
# Hay que quitar todo lo que usaste para que no interfiera con la siguiente
# persona que quiera ejecutar código.
rm(data_month, df_long, df_long_filtered, df_sin_outliers, mercado_media, modelo, stats_por_arma, items_available)
detach("package:tidyverse", unload = TRUE)
detach("package:dplyr", unload = TRUE)
detach("package:tidyr", unload = TRUE)
detach("package:purrr", unload = TRUE)
detach("package:ggplot2", unload = TRUE)
detach("package:lubridate", unload = TRUE)
gc()
