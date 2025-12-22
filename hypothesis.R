library(tidyverse)

data_month <- readRDS("./out/data_monthly.rds")

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

# Código principal de las hipótesis -------------

set.seed(69)
nombres_X <- df_long_filtered %>% 
  filter(str_detect(nombre, "AK-47"), !str_detect(nombre, "Graffiti")) %>% 
  distinct(nombre) %>% sample_n(30)

nombres_Y <- df_long_filtered %>% 
  filter(str_detect(nombre, "Desert Eagle"), !str_detect(nombre, "Graffiti")) %>% 
  distinct(nombre) %>% sample_n(30)

precios_X <- df_long_filtered %>%
  filter(nombre %in% nombres_X$nombre) %>% filter(fecha == "2023-06") %>%
  pull(precio)

precios_Y <- df_long_filtered %>%
  filter(nombre %in% nombres_Y$nombre) %>% filter(fecha == "2023-06") %>%
  pull(precio)

# A la hora de hacer la comparación estadística respecto a la media, nuestra hipótesis será:
# Hipótesis nula (H0), el precio promedio de las AK-47 es igual al precio promedio de
# las Desert Eagles?
# Hipótesis alternativa (H1), ¿Es este precio promedio diferente?
# Al haber tantos datos/precios vamos a comparar únicamente los de una fecha aleatoria.

t.test(precios_X, precios_Y)

# Como podemos ver, el p-value (0.0848 > 0.05) por lo que no podemos rechazar la hipótesis nula.

# Ahora necesitamoss comparar la media, haremos una hipótesis similar:
# Hipótesis nula (H0), la variabilidad de las AK-47 es igual a la de
# las Desert Eagles?
# Hipótesis alternativa (H1), Las variabilidades son diferentes.

var.test(precios_X, precios_Y)

# En este caso, el p-value (2.2e-16 < 0.05) por lo que rechazamos la hipótesis nula a favor de la
# alternativa y confirmamos que las variabilidades son diferentes.