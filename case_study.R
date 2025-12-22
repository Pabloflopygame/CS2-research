# --- Helper: parsea una celda a (oferta, precio) numéricos o missing ---
.parse_cell <- function(cell) {
  # missing: NULL real, "NULL", NA, length 0
  if (is.null(cell) || length(cell) == 0 ||
      (length(cell) == 1 && (is.na(cell) || identical(cell, "NULL")))) {
    return(list(missing = TRUE, oferta = NA_real_, precio = NA_real_))
  }
  
  # Si viene como vector/list con nombres (oferta, precio)
  if ((is.atomic(cell) || is.list(cell)) && length(cell) >= 2) {
    nm <- names(cell)
    if (!is.null(nm) && all(c("oferta", "precio") %in% nm)) {
      return(list(
        missing = FALSE,
        oferta  = as.numeric(cell[["oferta"]]),
        precio  = as.numeric(cell[["precio"]])
      ))
    }
    # Si viene sin nombres pero con dos valores (oferta, precio)
    if (length(cell) == 2) {
      return(list(
        missing = FALSE,
        oferta = as.numeric(cell[[1]]),
        precio = as.numeric(cell[[2]])
      ))
    }
  }
  
  # Si viene como string "0.00, 0.27"
  if (is.character(cell) && length(cell) == 1) {
    parts <- strsplit(cell, ",", fixed = TRUE)[[1]]
    if (length(parts) >= 2) {
      return(list(
        missing = FALSE,
        oferta = as.numeric(trimws(parts[1])),
        precio = as.numeric(trimws(parts[2]))
      ))
    }
  }
  
  # Fallback: si hay algún formato raro
  warning("Formato de celda no reconocido; se deja como missing. str(cell): ",
          paste(capture.output(str(cell)), collapse = " "))
  list(missing = TRUE, oferta = NA_real_, precio = NA_real_)
}

fix_zero_prices_locf <- function(df, id_col = "nombre") {
  month_cols <- setdiff(names(df), id_col)
  month_cols <- month_cols[order(as.Date(paste0(month_cols, "-01")))]
  
  # Asegura list-cols (para poder guardar NULL y pares oferta/precio)
  for (col in month_cols) {
    if (!is.list(df[[col]])) df[[col]] <- as.list(df[[col]])
  }
  
  # Normaliza: cada celda -> NULL o c(oferta=..., precio=...) numérico
  df[month_cols] <- lapply(df[month_cols], function(col) {
    lapply(col, function(cell) {
      p <- .parse_cell(cell)
      if (p$missing) return(NULL)
      c(oferta = p$oferta, precio = p$precio)
    })
  })
  
  # Extrae matrices numéricas (rápido para aplicar LOCF por fila)
  oferta <- sapply(df[month_cols], function(col)
    vapply(col, function(cell) if (is.null(cell)) NA_real_ else unname(cell[["oferta"]]), numeric(1))
  )
  precio <- sapply(df[month_cols], function(col)
    vapply(col, function(cell) if (is.null(cell)) NA_real_ else unname(cell[["precio"]]), numeric(1))
  )
  
  # Corrige por fila: si oferta==0 & precio==0 => precio = último precio > 0 anterior
  for (i in seq_len(nrow(df))) {
    last_pos <- NA_real_
    for (j in seq_along(month_cols)) {
      o <- oferta[i, j]
      p <- precio[i, j]
      
      if (!is.na(p) && p > 0) last_pos <- p
      
      if (!is.na(o) && o == 0 && !is.na(p) && p == 0 && !is.na(last_pos)) {
        precio[i, j] <- last_pos
      }
    }
  }
  
  # Vuelca precios al df (manteniendo NULL intactos)
  for (j in seq_along(month_cols)) {
    colname <- month_cols[j]
    col <- df[[colname]]
    newp <- precio[, j]
    
    for (i in seq_along(col)) {
      cell <- col[[i]]
      if (is.null(cell)) next
      cell[["precio"]] <- newp[[i]]
      col[[i]] <- cell
    }
    df[[colname]] <- col
  }
  
  df
}


data_month <- readRDS("./out/data_monthly.rds")
data_month_fixed <- fix_zero_prices_locf(data_month)
df_long_filtered <- data_month_fixed %>%
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

# ====================== PROBABILIDADES Y CAJAS ============================

# Fracture Case

probs <- data.frame(category=c("Mil-Spec", "Restricted", "Classified", "Covert", "Special Item"), 
                    prob=c(0.79923, 0.15985, 0.03197, 0.00639, 0.00256))

wear <- list(
  names = c("Factory New", "Minimal Wear", "Field-Tested", "Well-Worn", "Battle-Scarred"),
  breaks = c(0.00, 0.07, 0.15, 0.38, 0.45, 1.00)
)

fracture_weapons <- c("Negev | Ultralight",
                      "P2000 | Gnarled",
                      "SG 553 | Ol' Rusty",
                      "SSG 08 | Mainframe 001",
                      "P250 | Cassette",
                      "P90 | Freight",
                      "PP-Bizon | Runic",
                      "MAG-7 | Monster Call",
                      "Tec-9 | Brother",
                      "MAC-10 | Allure",
                      "Galil AR | Connexion",
                      "MP5-SD | Kitbash",
                      "M4A4 | Tooth Fairy",
                      "Glock-18 | Vogue",
                      "XM1014 | Entombed",
                      "Desert Eagle | Printstream",
                      "AK-47 | Legion of Anubis",
                      "Special Item")

rarity <- c(7, 5, 3, 2, 1) # Cuantos items de la caja pertenecen a cada color (en orden de azul a rojo)

fracture_case <- data.frame(weapons=fracture_weapons, probs=rep(probs$prob/rarity, rarity))

fracture_case$min_wear = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 
                           0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00) # los desgastes minimos de las armas

fracture_case$max_wear = c(0.78, 1.00, 1.00, 1.00, 0.60, 1.00, 1.00, 1.00, 1.00, 1.00, 
                           0.80, 0.80, 0.73, 0.75, 0.50, 0.80, 0.70, 1.00) # los desgastes maximos de las armas

# NOTA: NO HE AÑADIDO A LA CAJA LOS CUCHILLOS PORQUE SON 52 Y HAY QUE METERLOS MANUALMENTE.
# POR LO TANTO EL CODIGO NO FUNCIONA BIEN SI SALE UN CUCHILLO EN EL SIMULADOR (saldrá NA en el precio y no se encuentra el nombre)

# ============== SIMULADOR DE GAMBLING =======================

gamble <- function(n_items, date) {
  samp <- data.frame(weapons=sample(fracture_case$weapons, n_items, replace=T, fracture_case$probs))
  
  samp$stat = runif(n_items) < 0.10 # Tiene StatTrak? (10% usan en el juego al parecer)
  
  indexes <- match(samp$weapons, fracture_case$weapons)
  samp$wear <- cut(fracture_case$min_wear[indexes] + (runif(nrow(samp)) * (fracture_case$max_wear[indexes] - fracture_case$min_wear[indexes])), 
                   wear$breaks, 
                   wear$names) # Calculamos el desgaste del arma y clasificamos ese desgaste
  
  
  df_date <- df_long_filtered %>% filter(fecha == date) # Tenemos que coger los precios de un mes particular
  
  samp$weapons <- ifelse(
    samp$weapons == "Special Item",
    
    sample((df_date %>% filter(grepl("Knife", nombre))) %>% pull(nombre), n_items),
    
    paste0(ifelse(samp$stat, "StatTrak™ ",  ""), samp$weapons, " (", samp$wear, ")")) # Construimos la string del arma con su desgaste y si tiene StatTrak
  
  samp$price <- (df_date %>% pull(precio))[match(samp$weapons, df_date %>% pull(nombre))]
  
  return(samp)
}

beneficios <- replicate(100, sum(gamble(10, "2023-03")$price) - 17.60 * 10) # 50 simulaciones en las que abres 10 cajas

ggplot() + geom_histogram(aes(x=beneficios[beneficios > 0], y=..density..), fill="darkgreen", bins = 60) +
  geom_histogram(aes(x=beneficios[beneficios < 0], y=-..density..), fill="darkred", bins = 60) +
  labs(title="Beneficios de abrir 10 cajas para 1000 simulaciones", x="Beneficios", y="Densidad") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)

(probs %>% pull(prob))[match("Special Item", probs$category)] * 0.1 * 0.07 * 100
