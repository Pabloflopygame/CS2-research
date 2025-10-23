# ----Cargamos Steam API key----
if (!file.exists("API_KEY.txt")) {
  stop("❌ El archivo API_KEY.txt no existe.")
}

line <- readLines("API_KEY.txt")
value <- sub("STEAM_API_KEY=", "", line)
Sys.setenv(STEAM_API_KEY = value)

# borramos de "memoria" por seguridad
rm(line)
rm(value)

if (nzchar(Sys.getenv("STEAM_API_KEY"))) {
  message("✅ Clave API cargada correctamente.")
  # Sys.getenv("STEAM_API_KEY") <-- Para comprobar el valor
  # Si aparece un aviso "incomplete final line" simplemente 
  # crea una nueva linea debajo.
} else {
  warning("⚠️ No se pudo establecer la variable de entorno STEAM_API_KEY.")
}

# ----Crear ficheros----
# Para que no creé nada fuera de nuestro entorno
setwd(".")

# Carpeta destino
out_dir <- "out"

# Lista de ficheros a crear
files <- c(
  "seed_ids.csv",
  "user_edges.csv",
  "user_nodes.csv",
  "final_user.csv",
  "final_items.csv"
)

# Crear carpeta si no existe
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
  message("📁 Carpeta 'out' creada.")
} else {
  message("📁 Carpeta 'out' estaba creada.")
}

# Comprobamos si existe un fichero output NO vacío
for (f in files) {
  full_path <- file.path(out_dir, f)
  
  if (file.exists(full_path) && file.info(full_path)$size > 0) {
    stop(
      paste0(
        "❌ No se ha creado el fichero '", f,
        "' porque ya existe y contiene datos.\n",
        "   Continuar podría causar errores/pérdida de datos. 
        Mueve los datos a un lugar seguro antes de re-intentarlo."
      )
    )
  }
}

# Crear ficheros vacíos
for (f in files) {
  full_path <- file.path(out_dir, f)
  if (!file.exists(full_path)) {
    file.create(full_path)
    message("✅ Creado: ", full_path)
  } else {
    message("ℹ️ Fichero existente pero vacío, no se sobrescribe: ", full_path)
  }
}

# borramos datos inecesarios de memoria 
# (el resto mantendremos para futuro uso en los scripts)
rm(f)
rm(full_path)

message("\n🎉 Ficheros correctamente inicializados")

# ----Ejecutamos el seed extractor----
message("\n 🌱 Ejecutamos seed script")
source("scripts/seed_extractor.r")
message(" 🌳 seed script ejecutado")

# ----Eliminamos Steam Key de memoria----
# Importante ejecutar antes de mandar a github!
Sys.unsetenv("STEAM_API_KEY")
message("\n 🧹 Steam API key eliminada de la memoria.")
