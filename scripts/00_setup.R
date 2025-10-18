
# scripts/00_setup.R
packs <- c("httr2","jsonlite","xml2","dplyr","purrr","tibble","tidyr",
           "stringr","igraph","lubridate","readr")
inst <- rownames(installed.packages())
need <- setdiff(packs, inst)
if (length(need)) install.packages(need, repos = "https://cloud.r-project.org")
dir.create("../data", showWarnings = FALSE)
dir.create("../out", showWarnings = FALSE)
dir.create("../logs", showWarnings = FALSE)
message("Setup listo. Paquetes instalados: ", paste(packs, collapse=", "))

# cargamos steam API key
if (file.exists("../.Renviron")) readRenviron("../.Renviron")