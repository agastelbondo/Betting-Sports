# ============================================================
# Filtrar columnas de E0.csv segun un diccionario en Excel
# (SIN renombrar: las columnas conservan su nombre original)
#
# Los archivos deben estar en la MISMA carpeta que este script:
#   - E0.csv            (base de datos)
#   - diccionario.xlsx  (variables en la columna A, fila 1 = titulo)
# Resultado: carpeta 'resultados/' con E0_filtrado.csv
# ============================================================

# install.packages(c("readr", "readxl"))   # solo la primera vez
library(readr)
library(readxl)

# Usar como carpeta de trabajo la del script (RStudio). Si no usas RStudio,
# abre el script desde su carpeta o usa Session > Set Working Directory.
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

# ---- Lectura -----------------------------------------------
datos <- read_csv("E0.csv", guess_max = 10000, show_col_types = FALSE)
dic   <- read_excel("diccionario.xlsx")   # primera hoja, fila 1 = titulo
# Si tu lista empieza directamente en la fila 1 (sin titulo), usa:
# dic <- read_excel("diccionario.xlsx", col_names = FALSE)

variables <- as.character(dic[[1]])       # primera columna del Excel
variables <- variables[!is.na(variables) & trimws(variables) != ""]

# ---- Comparacion (tolerante) -------------------------------
# Ignora mayusculas, espacios normales y espacios especiales (\u00a0),
# asi "Home Team", "hometeam" y "HomeTeam " encuentran la columna HomeTeam.
normalizar <- function(x) tolower(gsub("[[:space:]\u00a0\ufeff]+", "", x))

posicion <- match(normalizar(variables), normalizar(names(datos)))

encontradas    <- unique(names(datos)[posicion[!is.na(posicion)]])
no_encontradas <- unique(variables[is.na(posicion)])

if (length(no_encontradas) > 0) {
  warning("Variables del diccionario que NO existen en el CSV (omitidas):\n  ",
          paste0("'", no_encontradas, "'", collapse = ", "))
}
if (length(encontradas) == 0) {
  stop("Ninguna variable del diccionario coincide con las columnas del CSV.")
}

cat("Columnas seleccionadas:", paste(encontradas, collapse = ", "), "\n")

# ---- Filtrar y guardar en una carpeta aparte ---------------
carpeta_salida <- "resultados"                   # cambia el nombre si quieres
dir.create(carpeta_salida, showWarnings = FALSE) # la crea si no existe

archivo_salida <- file.path(carpeta_salida, "E0_filtrado.csv")
write_csv(datos[, encontradas], archivo_salida)  # respeta el orden del diccionario

cat("Listo:", length(encontradas), "columnas y", nrow(datos),
    "filas guardadas en", archivo_salida, "\n")