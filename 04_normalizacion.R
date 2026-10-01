# ============================================================
# PASO 4. NORMALIZACION DE LAS PROBABILIDADES
# Responsable: Integrante 1
#
# Entrada:  resultados/probabilidades_margenes_largo.csv
#           (generado por margen_mercado.R: columnas
#            prob_local, prob_empate, prob_visitante, margen)
#
# Metodos aplicados (el profesor exige al menos dos; se incluye
# un tercero porque los metodos 1 y 2 no garantizan ambas cosas
# a la vez -> suma EXACTA a 1 y ausencia de negativos):
#
#   1) Normalizacion MULTIPLICATIVA
#        P_i^(1) = P_i / (P_L + P_E + P_V)
#
#   2) Normalizacion ADITIVA
#        P_i^(2) = P_i - M/3
#      (puede producir probabilidades negativas cuando alguna
#       P_i es menor que M/3; se documenta explicitamente)
#
#   3) Normalizacion LOGARITMICA (potencial)
#        Se busca un exponente n tal que:
#            P_L^n + P_E^n + P_V^n = 1
#        El exponente se resuelve con Newton-Raphson trabajando
#        en escala logaritmica:
#            f(n)  = sum( P_i^n ) - 1
#            f'(n) = sum( P_i^n * ln(P_i) )
#            n_{k+1} = n_k - f(n_k) / f'(n_k)
#        y las probabilidades normalizadas quedan:
#            P_i^(log) = P_i^n
#        Esta forma SIEMPRE suma exactamente 1 y SIEMPRE es
#        positiva (una potencia de un numero positivo nunca es
#        negativa), por lo que es la alternativa valida que pide
#        el enunciado para cuando el metodo aditivo falla.
#
# Salidas en resultados/:
#   probabilidades_normalizadas.csv  (una fila por partido-casa,
#                                      con los tres metodos)
#   resumen_normalizacion.csv        (comparacion y validacion)
# ============================================================

library(readr)
library(dplyr)

if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

# ---- 1. PARAMETROS -----------------------------------------
carpeta         <- "resultados"
archivo_entrada <- file.path(carpeta, "probabilidades_margenes_largo.csv")
tol             <- 1e-8   # tolerancia para validar que la suma = 1

if (!file.exists(archivo_entrada)) {
  stop("No existe ", archivo_entrada,
       ". Ejecuta primero margen_mercado.R")
}

# El archivo se guardo con write_excel_csv2 (separador ';', coma decimal)
datos <- read_csv2(archivo_entrada, guess_max = 10000, show_col_types = FALSE)

# Solo tiene sentido normalizar filas con las 3 probabilidades validas
datos_validos <- datos %>% filter(!is.na(prob_local) & !is.na(prob_empate) &
                                   !is.na(prob_visitante) & !is.na(margen))

cat("Filas totales:", nrow(datos), " | Filas validas para normalizar:",
    nrow(datos_validos), "\n\n")

PL <- datos_validos$prob_local
PE <- datos_validos$prob_empate
PV <- datos_validos$prob_visitante
M  <- datos_validos$margen

# ---- 2. METODO 1: NORMALIZACION MULTIPLICATIVA --------------
suma_bruta <- PL + PE + PV
PL_mult <- PL / suma_bruta
PE_mult <- PE / suma_bruta
PV_mult <- PV / suma_bruta

# ---- 3. METODO 2: NORMALIZACION ADITIVA ----------------------
PL_adit <- PL - M / 3
PE_adit <- PE - M / 3
PV_adit <- PV - M / 3

adit_negativo <- (PL_adit < 0) | (PE_adit < 0) | (PV_adit < 0)
n_negativos   <- sum(adit_negativo)

cat("Metodo aditivo -> partidos con al menos una probabilidad negativa:",
    n_negativos, "de", nrow(datos_validos),
    sprintf("(%.2f%%)\n\n", 100 * n_negativos / nrow(datos_validos)))

# ---- 4. METODO 3: NORMALIZACION LOGARITMICA (potencial) ------
# Resuelve, para CADA partido a la vez (vectorizado), el exponente
# n tal que PL^n + PE^n + PV^n = 1, mediante Newton-Raphson en
# escala logaritmica.
normalizar_logaritmico <- function(PL, PE, PV, iter_max = 100, tol_n = 1e-12) {
  n <- rep(1, length(PL))               # valor inicial
  logPL <- log(PL); logPE <- log(PE); logPV <- log(PV)

  for (k in seq_len(iter_max)) {
    PLn <- exp(n * logPL)
    PEn <- exp(n * logPE)
    PVn <- exp(n * logPV)

    f  <- PLn + PEn + PVn - 1
    fp <- PLn * logPL + PEn * logPE + PVn * logPV   # derivada (log-escala)

    # Evita division por cero / pasos invalidos
    fp[abs(fp) < 1e-12] <- -1e-12

    paso <- f / fp
    n_nuevo <- n - paso

    if (max(abs(n_nuevo - n), na.rm = TRUE) < tol_n) {
      n <- n_nuevo
      break
    }
    n <- n_nuevo
  }

  list(n = n,
       PL = exp(n * logPL),
       PE = exp(n * logPE),
       PV = exp(n * logPV))
}

res_log <- normalizar_logaritmico(PL, PE, PV)
PL_log <- res_log$PL
PE_log <- res_log$PE
PV_log <- res_log$PV
n_exponente <- res_log$n

# ---- 5. VALIDACION: las tres probabilidades deben sumar 1 ----
suma_mult <- PL_mult + PE_mult + PV_mult
suma_adit <- PL_adit + PE_adit + PV_adit
suma_log  <- PL_log  + PE_log  + PV_log

valida <- function(suma, nombre) {
  ok <- max(abs(suma - 1), na.rm = TRUE) < tol
  cat(sprintf("Metodo %-15s -> suma maxima desviacion de 1: %.2e  %s\n",
              nombre, max(abs(suma - 1), na.rm = TRUE),
              ifelse(ok, "[OK]", "[REVISAR]")))
  ok
}

cat("---- Validacion de normalizacion (suma debe ser 1) ----\n")
valida(suma_mult, "Multiplicativo")
valida(suma_adit, "Aditivo")
valida(suma_log,  "Logaritmico")
cat("\n")

# ---- 6. TABLA DE SALIDA: UNA FILA POR PARTIDO-CASA -----------
salida <- datos_validos %>%
  select(Date, HomeTeam, AwayTeam, casa, tipo,
         prob_local, prob_empate, prob_visitante, margen) %>%
  mutate(
    # Metodo 1: multiplicativo
    PL_mult = PL_mult, PE_mult = PE_mult, PV_mult = PV_mult,
    suma_mult = suma_mult,
    # Metodo 2: aditivo
    PL_adit = PL_adit, PE_adit = PE_adit, PV_adit = PV_adit,
    suma_adit = suma_adit,
    adit_tiene_negativos = adit_negativo,
    # Metodo 3: logaritmico
    exponente_n = n_exponente,
    PL_log = PL_log, PE_log = PE_log, PV_log = PV_log,
    suma_log = suma_log
  )

# ---- 7. RESUMEN COMPARATIVO ENTRE METODOS --------------------
resumen <- salida %>%
  summarise(
    n_partidos                 = n(),
    pct_negativos_aditivo      = round(100 * mean(adit_tiene_negativos), 3),
    diff_media_mult_vs_log_L   = round(mean(abs(PL_mult - PL_log)), 5),
    diff_media_mult_vs_log_E   = round(mean(abs(PE_mult - PE_log)), 5),
    diff_media_mult_vs_log_V   = round(mean(abs(PV_mult - PV_log)), 5),
    diff_media_adit_vs_log_L   = round(mean(abs(PL_adit - PL_log)), 5),
    diff_media_adit_vs_log_E   = round(mean(abs(PE_adit - PE_log)), 5),
    diff_media_adit_vs_log_V   = round(mean(abs(PV_adit - PV_log)), 5),
    exponente_n_promedio       = round(mean(exponente_n), 5),
    exponente_n_min            = round(min(exponente_n), 5),
    exponente_n_max            = round(max(exponente_n), 5)
  )

cat("---- Resumen comparativo ----\n")
print(t(resumen))

# ---- 8. GUARDAR ARCHIVOS -------------------------------------
write_excel_csv2(salida,  file.path(carpeta, "probabilidades_normalizadas.csv"))
write_excel_csv2(resumen, file.path(carpeta, "resumen_normalizacion.csv"))

cat("\nArchivos guardados en '", carpeta, "/':\n", sep = "")
cat(" - probabilidades_normalizadas.csv\n")
cat(" - resumen_normalizacion.csv\n")
