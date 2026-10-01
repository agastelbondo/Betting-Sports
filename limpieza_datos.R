# ============================================================
# LIMPIEZA DE DATOS - Premier League 2021/22 (E0.csv)
#
# Archivo de entrada:
#   - resultados/E0_filtrado.csv  (salida de filtrar_variables_sin_renombrar.R,
#     con los nombres ORIGINALES de columnas). Asi solo se limpian y se usan
#     las variables (y casas de apuestas) que elegiste en tu diccionario.
#
# Carpeta de salida: resultados/
#   - E0_limpio.csv              partidos que pasan todas las reglas
#   - matriz_limpieza.csv        una fila por partido: que reglas cumple
#   - resumen_limpieza.csv       cuantos cumplen / se eliminan por regla
#   - faltantes_por_variable.csv porcentaje de datos faltantes por columna
# ============================================================

# install.packages("readr")   # solo la primera vez
library(readr)

if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

# ---- 1. PARAMETROS -----------------------------------------
# Las cuotas de cierre (XCH, XCD, XCA) se detectan automaticamente entre las
# columnas de tu archivo filtrado: solo se validan las casas que elegiste.

# Variables clave del partido (para duplicados y consistencia de goles)
vars_clave <- c("Date", "HomeTeam", "AwayTeam",
                "FTHG", "FTAG", "FTR", "HTHG", "HTAG")

# ---- 2. LEER LA BASE FILTRADA ------------------------------
archivo_entrada <- file.path("resultados/filtradas", "E0_filtrado.csv")
if (!file.exists(archivo_entrada)) {
  stop("No existe ", archivo_entrada,
       ". Ejecuta primero filtrar_variables_sin_renombrar.R")
}
datos <- read_csv(archivo_entrada, guess_max = 10000,
                  na = c("", "NA"), show_col_types = FALSE)
n_original <- nrow(datos)

vars_cierre <- grep("^.+C[HDA]$", names(datos), value = TRUE)
if (length(vars_cierre) == 0) {
  stop("No hay cuotas de cierre (XCH, XCD, XCA) en ", archivo_entrada)
}
cat("Cuotas de cierre a validar:", paste(vars_cierre, collapse = ", "), "\n")

faltan <- setdiff(vars_clave, names(datos))
if (length(faltan) > 0) {
  stop("Estas variables necesarias no estan en ", archivo_entrada,
       " (agregalas a tu diccionario): ", paste(faltan, collapse = ", "))
}

fila_original <- seq_len(n_original)   # identificador de cada registro

# ---- 3. DATOS FALTANTES (sobre la base original) -----------
na_por_variable <- data.frame(
  variable    = names(datos),
  n_faltantes = as.integer(colSums(is.na(datos))),
  pct_faltante = round(100 * colSums(is.na(datos)) / n_original, 2)
)

vars_analisis <- c(vars_clave, vars_cierre)
fila_con_na_total    <- rowSums(is.na(datos)) > 0
fila_con_na_analisis <- rowSums(is.na(datos[, vars_analisis])) > 0

# ---- 4. REGLA 1: DUPLICADOS --------------------------------
# Un registro es duplicado si repite TODA la fila, o si repite el mismo
# partido (Date + HomeTeam + AwayTeam). Se conserva la primera aparicion.
dup_exacto <- duplicated(datos)
dup_partido <- duplicated(datos[, c("Date", "HomeTeam", "AwayTeam")])
regla_sin_duplicado <- !(dup_exacto | dup_partido)

# ---- 5. REGLA 2: CUOTAS DE CIERRE DECIMALES > 1 ------------
# Convierte a numero (texto no numerico pasa a NA) y exige valor finito > 1.
cuotas <- as.matrix(as.data.frame(
  lapply(datos[, vars_cierre], function(x) suppressWarnings(as.numeric(x)))
))
regla_cuotas <- rowSums(is.finite(cuotas) & cuotas > 1, na.rm = TRUE) ==
  length(vars_cierre)

# ---- 6. REGLA 3: GOLES AL MEDIO TIEMPO CONSISTENTES --------
num <- function(x) suppressWarnings(as.numeric(x))
FTHG <- num(datos$FTHG); FTAG <- num(datos$FTAG)
HTHG <- num(datos$HTHG); HTAG <- num(datos$HTAG)

gol_valido <- function(x) !is.na(x) & x >= 0 & x == floor(x)

# Medio tiempo <= final, para local y visitante
regla_medio_tiempo <- gol_valido(FTHG) & gol_valido(FTAG) &
  gol_valido(HTHG) & gol_valido(HTAG) &
  HTHG <= FTHG & HTAG <= FTAG

# ---- 7. REGLA 4: RESULTADO FINAL (FTR) COHERENTE CON GOLES --
esperado <- ifelse(FTHG > FTAG, "H", ifelse(FTHG < FTAG, "A", "D"))
ftr <- toupper(trimws(as.character(datos$FTR)))
regla_resultado <- !is.na(esperado) & !is.na(ftr) & ftr == esperado
regla_resultado[is.na(regla_resultado)] <- FALSE

# ---- 8. MATRIZ DE CUMPLIMIENTO POR PARTIDO -----------------
reglas <- data.frame(
  sin_duplicado      = regla_sin_duplicado,
  cuotas_validas     = regla_cuotas,
  medio_tiempo_ok    = regla_medio_tiempo,
  resultado_final_ok = regla_resultado
)

conservado <- rowSums(reglas) == ncol(reglas)

motivos <- cbind(
  ifelse(!reglas$sin_duplicado,      "duplicado", ""),
  ifelse(!reglas$cuotas_validas,     "cuota_invalida_o_faltante", ""),
  ifelse(!reglas$medio_tiempo_ok,    "goles_medio_tiempo_inconsistentes", ""),
  ifelse(!reglas$resultado_final_ok, "resultado_final_inconsistente", "")
)
motivo_eliminacion <- apply(motivos, 1, function(x) paste(x[x != ""], collapse = "; "))

matriz <- data.frame(
  fila_original = fila_original,
  Date          = datos$Date,
  HomeTeam      = datos$HomeTeam,
  AwayTeam      = datos$AwayTeam,
  # 1 = cumple la regla, 0 = no cumple
  as.data.frame(lapply(reglas, as.integer)),
  conservado    = as.integer(conservado),
  motivo_eliminacion = motivo_eliminacion
)

# ---- 9. RESUMEN POR REGLA ----------------------------------
# "incumplen" cuenta cada regla por separado (un partido puede fallar varias).
# "eliminados_en_secuencia" aplica las reglas en orden, sin contar dos veces.
restantes <- rep(TRUE, n_original)
resumen <- data.frame()
for (regla in names(reglas)) {
  elim <- restantes & !reglas[[regla]]
  restantes <- restantes & reglas[[regla]]
  resumen <- rbind(resumen, data.frame(
    regla = regla,
    cumplen = sum(reglas[[regla]]),
    incumplen = sum(!reglas[[regla]]),
    eliminados_en_secuencia = sum(elim),
    quedan_despues = sum(restantes)
  ))
}
resumen <- rbind(resumen, data.frame(
  regla = "TOTAL (todas las reglas)",
  cumplen = sum(conservado),
  incumplen = sum(!conservado),
  eliminados_en_secuencia = sum(!conservado),
  quedan_despues = sum(conservado)
))

# ---- 10. GUARDAR RESULTADOS --------------------------------
carpeta_salida <- "resultados/datos_limpios"
dir.create(carpeta_salida, showWarnings = FALSE)

datos_limpios <- datos[conservado, ]

# Base limpia: separador coma y punto decimal (para seguir trabajando en R)
write_csv(datos_limpios, file.path(carpeta_salida, "E0_limpio.csv"))

# Tablas de reporte: formato para Excel en espanol (; y coma decimal, UTF-8)
write_excel_csv2(matriz,          file.path(carpeta_salida, "matriz_limpieza.csv"))
write_excel_csv2(resumen,         file.path(carpeta_salida, "resumen_limpieza.csv"))
write_excel_csv2(na_por_variable, file.path(carpeta_salida, "faltantes_por_variable.csv"))

# ---- 11. RESPUESTAS A LAS PREGUNTAS ------------------------
n_limpio <- nrow(datos_limpios)

cat("\n================ RESULTADOS ================\n")
cat("1) Partidos en la base original:      ", n_original, "\n")
cat("2) Partidos despues de la limpieza:   ", n_limpio,
    sprintf("(%.1f%% de la base; se eliminaron %d)\n",
            100 * n_limpio / n_original, n_original - n_limpio))
cat("3) Registros con informacion faltante:\n")
cat(sprintf("   - En cualquier columna del archivo:         %d de %d (%.2f%%)\n",
            sum(fila_con_na_total), n_original,
            100 * mean(fila_con_na_total)))
cat(sprintf("   - En las variables usadas en el analisis:   %d de %d (%.2f%%)\n",
            sum(fila_con_na_analisis), n_original,
            100 * mean(fila_con_na_analisis)))
cat("\nResumen por regla:\n")
print(resumen, row.names = FALSE)
cat("\nArchivos guardados en la carpeta '", carpeta_salida, "/'\n", sep = "")
