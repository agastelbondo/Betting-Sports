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
carpeta1         <- "resultados/margen"
carpeta <- "resultados/normalizacion"
dir.create(carpeta, showWarnings = FALSE)
archivo_entrada <- file.path(carpeta1, "probabilidades_margenes_largo.csv")
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

cat("---- Resumen comparativo (estadisticos generales) ----\n")
print(t(resumen))
cat("\n")

# ---- 8. COMPARACION DE LOS 3 METODOS, PARTIDO POR PARTIDO ----
# Umbral para considerar que dos metodos dieron un resultado
# "similar" en un partido: diferencia absoluta maxima entre las
# tres probabilidades (L, E, V) menor al umbral (en puntos de
# probabilidad). Ajustable segun el criterio del equipo.
umbral_similitud <- 0.01   # 0.01 = 1 punto porcentual

comparacion <- salida %>%
  transmute(
    Date, HomeTeam, AwayTeam, casa, tipo,

    # Diferencias absolutas: Multiplicativo vs Aditivo
    dif_mult_adit_L = abs(PL_mult - PL_adit),
    dif_mult_adit_E = abs(PE_mult - PE_adit),
    dif_mult_adit_V = abs(PV_mult - PV_adit),

    # Diferencias absolutas: Multiplicativo vs Logaritmico
    dif_mult_log_L  = abs(PL_mult - PL_log),
    dif_mult_log_E  = abs(PE_mult - PE_log),
    dif_mult_log_V  = abs(PV_mult - PV_log),

    # Diferencias absolutas: Aditivo vs Logaritmico
    dif_adit_log_L  = abs(PL_adit - PL_log),
    dif_adit_log_E  = abs(PE_adit - PE_log),
    dif_adit_log_V  = abs(PV_adit - PV_log)
  ) %>%
  mutate(
    # Mayor diferencia encontrada entre CUALQUIER par de metodos
    # y CUALQUIER resultado (L, E o V) para ese partido
    diferencia_maxima = pmax(dif_mult_adit_L, dif_mult_adit_E, dif_mult_adit_V,
                              dif_mult_log_L,  dif_mult_log_E,  dif_mult_log_V,
                              dif_adit_log_L,  dif_adit_log_E,  dif_adit_log_V),
    clasificacion = ifelse(diferencia_maxima < umbral_similitud,
                            "Similar", "Diferente")
  )

n_similares  <- sum(comparacion$clasificacion == "Similar")
n_diferentes <- sum(comparacion$clasificacion == "Diferente")
cat(sprintf("Comparacion por partido -> Similares: %d (%.2f%%) | Diferentes: %d (%.2f%%)  [umbral = %.3f]\n\n",
            n_similares, 100 * n_similares / nrow(comparacion),
            n_diferentes, 100 * n_diferentes / nrow(comparacion),
            umbral_similitud))

# ---- 9. RESUMEN DE LA COMPARACION (similitudes/diferencias) --
resumen_comparacion <- comparacion %>%
  summarise(
    n_partidos            = n(),
    umbral_similitud_usado = umbral_similitud,
    n_similares           = n_similares,
    n_diferentes          = n_diferentes,
    pct_similares         = round(100 * n_similares / n(), 3),
    pct_diferentes        = round(100 * n_diferentes / n(), 3),

    dif_prom_mult_adit    = round(mean((dif_mult_adit_L + dif_mult_adit_E + dif_mult_adit_V) / 3), 5),
    dif_max_mult_adit     = round(max(pmax(dif_mult_adit_L, dif_mult_adit_E, dif_mult_adit_V)), 5),

    dif_prom_mult_log     = round(mean((dif_mult_log_L + dif_mult_log_E + dif_mult_log_V) / 3), 5),
    dif_max_mult_log      = round(max(pmax(dif_mult_log_L, dif_mult_log_E, dif_mult_log_V)), 5),

    dif_prom_adit_log     = round(mean((dif_adit_log_L + dif_adit_log_E + dif_adit_log_V) / 3), 5),
    dif_max_adit_log      = round(max(pmax(dif_adit_log_L, dif_adit_log_E, dif_adit_log_V)), 5),

    par_mas_parecido  = c("Mult-Adit", "Mult-Log", "Adit-Log")[
      which.min(c(mean((dif_mult_adit_L+dif_mult_adit_E+dif_mult_adit_V)/3),
                  mean((dif_mult_log_L +dif_mult_log_E +dif_mult_log_V) /3),
                  mean((dif_adit_log_L +dif_adit_log_E +dif_adit_log_V) /3)))],
    par_mas_distinto  = c("Mult-Adit", "Mult-Log", "Adit-Log")[
      which.max(c(mean((dif_mult_adit_L+dif_mult_adit_E+dif_mult_adit_V)/3),
                  mean((dif_mult_log_L +dif_mult_log_E +dif_mult_log_V) /3),
                  mean((dif_adit_log_L +dif_adit_log_E +dif_adit_log_V) /3)))]
  )

cat("---- Resumen de la comparacion entre metodos ----\n")
print(t(resumen_comparacion))
cat("\n")

# ---- 10. GRAFICA COMPARATIVA DE LOS 3 METODOS -----------------
# Un panel por par de metodos (Mult vs Adit, Mult vs Log, Adit vs
# Log). Dentro de cada panel, el eje X agrupa por CASA DE
# APUESTAS, y para cada casa se muestran 3 cajas de color (Local,
# Empate, Visitante) con la diferencia absoluta entre esos dos
# metodos. Asi se ve, por casa Y por tipo de resultado, que tan
# parecidos o distintos son los metodos entre si.
casas_ordenadas <- sort(unique(comparacion$casa))

construir_bloque <- function(par_m, col_L, col_E, col_V) {
  rbind(
    data.frame(casa = comparacion$casa, resultado = "Local",
               par_metodo = par_m, diferencia = col_L),
    data.frame(casa = comparacion$casa, resultado = "Empate",
               par_metodo = par_m, diferencia = col_E),
    data.frame(casa = comparacion$casa, resultado = "Visitante",
               par_metodo = par_m, diferencia = col_V)
  )
}

datos_grafica <- rbind(
  construir_bloque("Mult vs Adit", comparacion$dif_mult_adit_L,
                    comparacion$dif_mult_adit_E, comparacion$dif_mult_adit_V),
  construir_bloque("Mult vs Log",  comparacion$dif_mult_log_L,
                    comparacion$dif_mult_log_E,  comparacion$dif_mult_log_V),
  construir_bloque("Adit vs Log",  comparacion$dif_adit_log_L,
                    comparacion$dif_adit_log_E,  comparacion$dif_adit_log_V)
)

datos_grafica$par_metodo <- factor(datos_grafica$par_metodo,
                                    levels = c("Mult vs Adit", "Mult vs Log", "Adit vs Log"))
datos_grafica$resultado  <- factor(datos_grafica$resultado,
                                    levels = c("Local", "Empate", "Visitante"))
datos_grafica$casa       <- factor(datos_grafica$casa, levels = casas_ordenadas)

colores_resultado <- c(Local = "steelblue", Empate = "goldenrod3", Visitante = "indianred")

graficar_comparacion_metodos <- function() {
  par(mfrow = c(1, 3), oma = c(0, 0, 3, 0), mar = c(6, 4, 3, 1))
  for (par_m in levels(datos_grafica$par_metodo)) {
    sub <- datos_grafica[datos_grafica$par_metodo == par_m, ]
    # resultado cicla mas rapido que casa -> 3 cajas (L,E,V) por casa
    boxplot(diferencia ~ resultado + casa, data = sub,
            col = rep(colores_resultado, times = length(casas_ordenadas)),
            main = par_m, xlab = "", ylab = "Diferencia absoluta",
            las = 2, cex.axis = 0.6, xaxt = "n")
    axis(1, at = seq(2, 3 * length(casas_ordenadas), by = 3),
         labels = casas_ordenadas, las = 2, cex.axis = 0.8)
    if (par_m == levels(datos_grafica$par_metodo)[1]) {
      legend("topleft", inset = c(0.02, 0.05), legend = names(colores_resultado),
             fill = colores_resultado, bty = "n", cex = 0.75)
    }
  }
  mtext("Comparacion de los 3 metodos de normalizacion por casa de apuestas (Local / Empate / Visitante)",
        outer = TRUE, cex = 1, font = 2)
  par(mfrow = c(1, 1))
}

png(file.path(carpeta, "comparacion_metodos.png"), width = 1900, height = 650, res = 120)
graficar_comparacion_metodos(); dev.off()

graficar_comparacion_metodos()   # tambien se muestra en el panel de graficos

# ---- 11. GUARDAR LOS 4 ARCHIVOS CSV ---------------------------
write_excel_csv2(salida,              file.path(carpeta, "probabilidades_normalizadas.csv"))
write_excel_csv2(resumen,             file.path(carpeta, "resumen_normalizacion.csv"))
write_excel_csv2(comparacion,         file.path(carpeta, "comparacion_metodos_por_partido.csv"))
write_excel_csv2(resumen_comparacion, file.path(carpeta, "resumen_comparacion_metodos.csv"))

cat("\nArchivos guardados en '", carpeta, "/':\n", sep = "")
cat(" - probabilidades_normalizadas.csv       (las 3 probabilidades normalizadas por partido-casa)\n")
cat(" - resumen_normalizacion.csv             (estadisticos generales por metodo: % negativos, exponente n, etc.)\n")
cat(" - comparacion_metodos_por_partido.csv   (diferencias entre los 3 metodos, partido por partido)\n")
cat(" - resumen_comparacion_metodos.csv       (resumen: % de partidos similares/diferentes, que par de metodos se parece mas/menos)\n")
cat(" - comparacion_metodos.png               (boxplots comparando los 3 metodos por resultado L/E/V)\n")
