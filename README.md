
# Steam CS2 Research (R)

Pipeline modular para:
1) **Seeding** desde grupos de Steam (XML)
2) **Exploración BFS** de amigos hasta cierta profundidad
3) **Detección de CS2** (app 730) y horas jugadas
4) **Inventario** CS2 (730/2) desde la comunidad
5) **Histórico de precios** (Buff.163) — subset
6) **Join** inventario + último precio disponible

## Requisitos
- R >= 4.1
- Paquetes R: `httr2`, `jsonlite`, `xml2`, `dplyr`, `purrr`, `tibble`, `tidyr`,
  `stringr`, `igraph`, `lubridate`, `readr`, opcionalmente `targets`.

## Configuración
1. Copia `.Renviron.example` a `.Renviron` y pon tu clave:
   ```
   STEAM_API_KEY=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
   ```
   También puedes exportarla en tu sesión:
   ```r
   Sys.setenv(STEAM_API_KEY="xxxxxxxx")
   ```
2. (Opcional) Ajusta parámetros vía variables de entorno:
   - `BFS_DEPTH` (por defecto 2)
   - `MAX_NODES` (por defecto 30000)
   - `BUFF_MAX_ITEMS` (por defecto 200)

## Ejecución rápida con Makefile
```bash
make setup     # instala/asegura paquetes y carpetas
make seeding   # 01: obtiene steamIDs desde grupos
make bfs       # 02: recorre amigos hasta profundidad BFS_DEPTH
make owned     # 03: comprueba CS2 (730) y horas
make inventory # 04: baja inventarios 730/2
make buff      # 05: baja/lee histórico Buff y submuestrea
make join      # 06: une inventario con precio reciente
make all       # corre todo en orden
```

## Ejecución alternativa con {targets}
```r
install.packages("targets")
targets::tar_make()  # reproducirá el pipeline definido en _targets.R
```

## Entradas
- `config/seed_groups.txt`: lista de URLs de grupos (una por línea).

## Salidas principales (`out/`)
- `seed_ids.csv` / `seed_ids.rds`
- `bfs_edges.csv`, `bfs_nodes.csv`, `bfs.rds`
- `users_cs2.csv`
- `inventory_730.csv`
- `buff_weekly_subset.csv`
- `inventory_730_priced.csv`

> Usa `logs/` para tus propios registros si los necesitas.
