# ============================================================
# BASE ANALITICA (.csv)
# Un registro por PARTIDO y OPERADOR (casa de apuestas), con:
#   - cuotas originales (cierre)
#   - probabilidades brutas
#   - probabilidades normalizadas por cada metodo aplicado
#       (multiplicativo, aditivo y logaritmico)
#   - margen calculado
#   - resultado observado
#
# Entradas (generadas por tus scripts anteriores):
#   resultados/normalizacion/probabilidades_normalizadas.csv  (normalizacion_probabilidades.R)
#   resultados/margen/probabilidades_margenes_largo.csv       (margen_mercado.R)
#   resultados/datos_limpios/E0_limpio.csv                    (limpieza_datos.R)
#
# Salidas en resultados/base_analitica/:
#   base_analitica.csv               coma como separador, punto decimal
#   base_analitica_excel.csv         ';' y coma decimal (para abrir en Excel en espanol)
#   diccionario_base_analitica.csv   descripcion de cada columna
# ============================================================

library(readr)
library(dplyr)

if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

# ---- 1. PARAMETROS -----------------------------------------
ruta_normalizacion <- file.path("resultados", "normalizacion", "probabilidades_normalizadas.csv")
ruta_margen        <- file.path("resultados", "margen", "probabilidades_margenes_largo.csv")
ruta_limpio        <- file.path("resultados", "datos_limpios", "E0_limpio.csv")
carpeta_salida     <- file.path("resultados", "base_analitica")
tol <- 1e-8   # tolerancia para validar que las probabilidades suman 1

for (f in c(ruta_normalizacion, ruta_margen, ruta_limpio)) {
  if (!file.exists(f)) stop("No existe ", f, ". Ejecuta antes los scripts anteriores del MAESTRO.")
}
dir.create(carpeta_salida, showWarnings = FALSE, recursive = TRUE)

clave_partido <- c("Date", "HomeTeam", "AwayTeam")
clave_operador <- c(clave_partido, "casa")

exigir <- function(df, columnas, nombre) {
  faltan <- setdiff(columnas, names(df))
  if (length(faltan) > 0) {
    stop("Faltan columnas en ", nombre, ": ", paste(faltan, collapse = ", "))
  }
}

# ---- 2. LEER LAS TRES FUENTES ------------------------------
# Date y los nombres se leen como texto para que la union no dependa del formato.
tipos_texto <- cols(Date = col_character(), HomeTeam = col_character(),
                    AwayTeam = col_character(), casa = col_character(),
                    .default = col_guess())

# normalizacion y margen se guardaron con write_excel_csv2 (';' y coma decimal)
norm   <- read_csv2(ruta_normalizacion, col_types = tipos_texto,
                    guess_max = 10000, show_col_types = FALSE)
margen <- read_csv2(ruta_margen, col_types = tipos_texto,
                    guess_max = 10000, show_col_types = FALSE)
# la base limpia se guardo con write_csv (',' y punto decimal)
limpio <- read_csv(ruta_limpio, col_types = tipos_texto,
                   guess_max = 10000, show_col_types = FALSE)

exigir(norm, c(clave_operador, "tipo", "prob_local", "prob_empate", "prob_visitante",
               "margen", "PL_mult", "PE_mult", "PV_mult", "suma_mult",
               "PL_adit", "PE_adit", "PV_adit", "suma_adit", "adit_tiene_negativos",
               "exponente_n", "PL_log", "PE_log", "PV_log", "suma_log"),
        "probabilidades_normalizadas.csv")
exigir(margen, c(clave_operador, "cuota_local", "cuota_empate", "cuota_visitante"),
       "probabilidades_margenes_largo.csv")
exigir(limpio, c(clave_partido, "FTHG", "FTAG", "FTR"),
       "E0_limpio.csv (agrega FTHG, FTAG y FTR a tu diccionario)")

# ---- 3. UNIR: CUOTAS ORIGINALES + RESULTADO OBSERVADO ------
cuotas <- margen %>%
  select(all_of(clave_operador), cuota_local, cuota_empate, cuota_visitante) %>%
  distinct(across(all_of(clave_operador)), .keep_all = TRUE)

observado <- limpio %>%
  select(all_of(clave_partido), FTHG, FTAG, FTR) %>%
  distinct(across(all_of(clave_partido)), .keep_all = TRUE)

base <- norm %>%
  left_join(cuotas,    by = clave_operador) %>%
  left_join(observado, by = clave_partido) %>%
  arrange(as.Date(Date, format = "%d/%m/%Y"), HomeTeam, casa) %>%
  transmute(
    # Identificacion
    Date, HomeTeam, AwayTeam, casa, tipo,
    # Cuotas originales de cierre
    cuota_local, cuota_empate, cuota_visitante,
    # Probabilidades brutas (1 / cuota) y margen
    prob_bruta_local     = prob_local,
    prob_bruta_empate    = prob_empate,
    prob_bruta_visitante = prob_visitante,
    suma_prob_bruta      = prob_local + prob_empate + prob_visitante,
    margen,
    # Normalizacion 1: multiplicativa
    prob_mult_local = PL_mult, prob_mult_empate = PE_mult, prob_mult_visitante = PV_mult,
    suma_mult,
    # Normalizacion 2: aditiva
    prob_adit_local = PL_adit, prob_adit_empate = PE_adit, prob_adit_visitante = PV_adit,
    suma_adit, adit_tiene_negativos,
    # Normalizacion 3: logaritmica (potencial)
    prob_log_local = PL_log, prob_log_empate = PE_log, prob_log_visitante = PV_log,
    exponente_n, suma_log,
    # Resultado observado
    goles_local = FTHG, goles_visitante = FTAG,
    resultado_observado = FTR,                       # H = local, D = empate, A = visitante
    obs_local     = as.integer(FTR == "H"),
    obs_empate    = as.integer(FTR == "D"),
    obs_visitante = as.integer(FTR == "A")
  )

# ---- 4. VALIDACIONES ---------------------------------------
cat("\n================ VALIDACION DE LA BASE ANALITICA ================\n")
cat("Registros (partido x casa):", nrow(base), "\n")
cat("Partidos distintos:        ", nrow(distinct(base, Date, HomeTeam, AwayTeam)), "\n")
cat("Casas de apuestas:         ", n_distinct(base$casa), "->",
    paste(sort(unique(base$casa)), collapse = ", "), "\n")

n_dup <- sum(duplicated(base[, clave_operador]))
cat("Registros duplicados (partido x casa):", n_dup, ifelse(n_dup == 0, "[OK]", "[REVISAR]"), "\n")

sin_cuotas <- sum(is.na(base$cuota_local) | is.na(base$cuota_empate) | is.na(base$cuota_visitante))
cat("Registros sin cuotas originales:      ", sin_cuotas, ifelse(sin_cuotas == 0, "[OK]", "[REVISAR]"), "\n")

sin_resultado <- sum(is.na(base$resultado_observado))
cat("Registros sin resultado observado:    ", sin_resultado, ifelse(sin_resultado == 0, "[OK]", "[REVISAR]"), "\n")

for (m in c("suma_mult", "suma_adit", "suma_log")) {
  desv <- max(abs(base[[m]] - 1), na.rm = TRUE)
  cat(sprintf("Max desviacion de 1 en %-10s: %.2e %s\n", m, desv,
              ifelse(desv < tol, "[OK]", "[REVISAR]")))
}

cat("\nRegistros por casa de apuestas:\n")
print(as.data.frame(count(base, casa)), row.names = FALSE)

cat("\nValores faltantes por columna (solo las que tienen):\n")
faltantes <- colSums(is.na(base))
if (any(faltantes > 0)) print(faltantes[faltantes > 0]) else cat("  Ninguno\n")

# ---- 5. DICCIONARIO DE LA BASE -----------------------------
diccionario <- data.frame(
  variable = names(base),
  descripcion = c(
    "Fecha del partido (dd/mm/aaaa)", "Equipo local", "Equipo visitante",
    "Operador (casa de apuestas o agregado Max/Avg)", "Tipo: Casa de apuestas / Agregado del mercado",
    "Cuota de cierre: victoria local", "Cuota de cierre: empate", "Cuota de cierre: victoria visitante",
    "Probabilidad bruta (1/cuota): local", "Probabilidad bruta: empate", "Probabilidad bruta: visitante",
    "Suma de las tres probabilidades brutas (>1 por el margen)",
    "Margen del mercado M = suma de probabilidades brutas - 1",
    "Prob. normalizada multiplicativa: local", "Prob. normalizada multiplicativa: empate",
    "Prob. normalizada multiplicativa: visitante", "Suma de las probabilidades multiplicativas (=1)",
    "Prob. normalizada aditiva (P - M/3): local", "Prob. normalizada aditiva: empate",
    "Prob. normalizada aditiva: visitante", "Suma de las probabilidades aditivas (=1)",
    "TRUE si el metodo aditivo produjo alguna probabilidad negativa",
    "Prob. normalizada logaritmica (P^n): local", "Prob. normalizada logaritmica: empate",
    "Prob. normalizada logaritmica: visitante",
    "Exponente n tal que la suma de P^n es 1", "Suma de las probabilidades logaritmicas (=1)",
    "Goles del local al final del partido", "Goles del visitante al final del partido",
    "Resultado observado: H = local, D = empate, A = visitante",
    "1 si gano el local, 0 si no", "1 si hubo empate, 0 si no", "1 si gano el visitante, 0 si no"
  ),
  stringsAsFactors = FALSE
)
stopifnot(nrow(diccionario) == ncol(base))   # asegura que cada columna tenga descripcion

# ---- 6. GUARDAR --------------------------------------------
write_csv(base, file.path(carpeta_salida, "base_analitica.csv"))
write_excel_csv2(base, file.path(carpeta_salida, "base_analitica_excel.csv"))
write_excel_csv2(diccionario, file.path(carpeta_salida, "diccionario_base_analitica.csv"))

cat("\nArchivos guardados en '", carpeta_salida, "/':\n", sep = "")
cat(" - base_analitica.csv (coma / punto decimal)\n")
cat(" - base_analitica_excel.csv (; / coma decimal, para Excel en espanol)\n")
cat(" - diccionario_base_analitica.csv\n")
