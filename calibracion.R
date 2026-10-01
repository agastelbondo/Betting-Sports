# ============================================================
# PRUEBA DE CALIBRACIÓN — CASA DE APUESTAS vs REALIDAD
#
# Entrada:
#   resultados/probabilidades_normalizadas.csv  (de 04_normalizacion.R)
#   resultados/E0_limpio.csv                    (de limpieza_datos.R)
#
# Métricas:
#   Brier Score (BS): error cuadrático medio — más bajo = mejor
#   ECE: Expected Calibration Error — más bajo = mejor
#
# Salidas en resultados/:
#   calibracion_predicciones.csv   (partido x casa: prob, resultado, acierto)
#   calibracion_intervalos.csv     (tabla por intervalo de probabilidad)
#   metricas_calibracion.csv       (BS y ECE por casa y resultado)
#   calibracion_[casa].png         (curva de calibración)
# ============================================================

library(readr)

if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

# ---- 1. PARÁMETROS ------------------------------------------
carpeta      <- "resultados/calibracion"
dir.create(carpeta, showWarnings = FALSE)
carpeta1 <- "resultados/normalizacion"
carpeta2 <- "resultados/datos_limpios"

arch_norm    <- file.path(carpeta1, "probabilidades_normalizadas.csv")
arch_orig    <- file.path(carpeta2, "E0_limpio.csv")
N_INTERVALOS <- 10

if (!file.exists(arch_norm)) stop("Ejecuta primero 04_normalizacion.R")
if (!file.exists(arch_orig)) stop("Ejecuta primero limpieza_datos.R")

norm  <- read_csv2(arch_norm, show_col_types = FALSE)
datos <- read_csv(arch_orig,  show_col_types = FALSE)

# ---- 2. UNIR RESULTADO REAL ---------------------------------
id_datos <- paste(datos$Date, datos$HomeTeam, datos$AwayTeam)
id_norm  <- paste(norm$Date,  norm$HomeTeam,  norm$AwayTeam)

norm$FTR            <- datos$FTR[match(id_norm, id_datos)]
norm$real_local     <- as.integer(norm$FTR == "H")
norm$real_empate    <- as.integer(norm$FTR == "D")
norm$real_visitante <- as.integer(norm$FTR == "A")

# Solo casas reales
norm <- norm[norm$tipo == "Casa de apuestas", ]

# ---- 3. FUNCIONES -------------------------------------------
brier_score <- function(p, y) {
  idx <- !is.na(p) & !is.na(y)
  mean((p[idx] - y[idx])^2)
}

tabla_cal <- function(p, y, n_int = N_INTERVALOS) {
  breaks    <- seq(0, 1, length.out = n_int + 1)
  idx       <- !is.na(p) & !is.na(y)
  p <- p[idx]; y <- y[idx]
  intervalo <- cut(p, breaks = breaks, include.lowest = TRUE)

  do.call(rbind, lapply(levels(intervalo), function(lv) {
    sel <- intervalo == lv
    if (sum(sel) == 0) return(NULL)
    data.frame(
      intervalo  = lv,
      n          = sum(sel),
      prob_media = round(mean(p[sel]), 4),
      freq_real  = round(mean(y[sel]), 4),
      diferencia = round(mean(p[sel]) - mean(y[sel]), 4),
      stringsAsFactors = FALSE
    )
  }))
}

ece_calc <- function(tab) {
  N <- sum(tab$n)
  sum((tab$n / N) * abs(tab$prob_media - tab$freq_real))
}

# ---- 4. TABLA PARTIDO X CASA: PREDICCIÓN VS REALIDAD --------
# Para cada partido y casa: probabilidades brutas, resultado
# predicho (mayor probabilidad), resultado real y acierto

casas <- unique(norm$casa)

tabla_predicciones <- do.call(rbind, lapply(casas, function(casa) {
  sub <- norm[norm$casa == casa, ]

  # Resultado que predice la casa = el de mayor probabilidad
  pred <- apply(
    cbind(sub$prob_local, sub$prob_empate, sub$prob_visitante),
    1,
    function(row) {
      if (any(is.na(row))) return(NA)
      c("H", "D", "A")[which.max(row)]
    }
  )

  # Nombre legible del resultado real
  resultado_real_txt <- ifelse(sub$FTR == "H", "Local",
                        ifelse(sub$FTR == "D", "Empate", "Visitante"))
  resultado_pred_txt <- ifelse(pred == "H", "Local",
                        ifelse(pred == "D", "Empate", "Visitante"))

  acierto <- ifelse(pred == sub$FTR, "\u2713", "\u2717")

  data.frame(
    Fecha           = sub$Date,
    Local           = sub$HomeTeam,
    Visitante       = sub$AwayTeam,
    Casa            = casa,
    Prob_Local      = round(as.numeric(sub$prob_local),     4),
    Prob_Empate     = round(as.numeric(sub$prob_empate),    4),
    Prob_Visitante  = round(as.numeric(sub$prob_visitante), 4),
    Prediccion_Casa = resultado_pred_txt,
    Resultado_Real  = resultado_real_txt,
    Acierto         = acierto,
    stringsAsFactors = FALSE
  )
}))

# Porcentaje de acierto por casa
cat("\n========== TABLA DE PREDICCIONES vs REALIDAD ==========\n")
cat("(Predicción = resultado con mayor probabilidad según la casa)\n\n")

for (casa in casas) {
  sub_pred <- tabla_predicciones[tabla_predicciones$Casa == casa, ]
  n_total  <- nrow(sub_pred)
  n_acierto <- sum(sub_pred$Acierto == "\u2713")
  cat(sprintf("Casa %s: %d aciertos de %d partidos (%.1f%%)\n",
              casa, n_acierto, n_total, 100 * n_acierto / n_total))
}

# Mostrar primeras 10 filas como muestra
cat("\n— Muestra (primeros 10 partidos, casa B365) —\n")
muestra <- tabla_predicciones[tabla_predicciones$Casa == "B365", ][1:10, ]
print(muestra, row.names = FALSE)

# ---- 5. MÉTRICAS BS y ECE -----------------------------------
resultados_def <- list(
  Local     = list(col_p = "prob_local",     col_y = "real_local"),
  Empate    = list(col_p = "prob_empate",    col_y = "real_empate"),
  Visitante = list(col_p = "prob_visitante", col_y = "real_visitante")
)

todos_intervalos <- list()
todas_metricas   <- list()

for (casa in casas) {
  sub <- norm[norm$casa == casa, ]

  for (res in names(resultados_def)) {
    p <- as.numeric(sub[[ resultados_def[[res]]$col_p ]])
    y <- as.numeric(sub[[ resultados_def[[res]]$col_y ]])

    tab           <- tabla_cal(p, y)
    tab$casa      <- casa
    tab$resultado <- res
    todos_intervalos[[paste(casa, res)]] <- tab

    todas_metricas[[paste(casa, res)]] <- data.frame(
      casa        = casa,
      resultado   = res,
      n_partidos  = sum(!is.na(p) & !is.na(y)),
      brier_score = round(brier_score(p, y), 5),
      ece         = round(ece_calc(tab), 5),
      stringsAsFactors = FALSE
    )
  }
}

tabla_met <- do.call(rbind, todas_metricas)
rownames(tabla_met) <- NULL

cat("\n========== MÉTRICAS DE CALIBRACIÓN ==========\n")
cat("Brier Score: más bajo = mejor (referencia sin info = 0.2222)\n")
cat("ECE        : más bajo = mejor (0 = calibración perfecta)\n\n")
print(tabla_met, row.names = FALSE)

cat("\n— ECE promedio por casa —\n")
resumen_ece <- aggregate(ece ~ casa, data = tabla_met, FUN = mean)
resumen_ece$ece <- round(resumen_ece$ece, 5)
print(resumen_ece, row.names = FALSE)

# ---- 6. GRÁFICAS --------------------------------------------
colores <- c(Local = "steelblue", Empate = "darkorange", Visitante = "forestgreen")

for (casa in casas) {
  archivo_png <- file.path(carpeta, paste0("calibracion_", tolower(casa), ".png"))
  png(archivo_png, width = 1400, height = 500, res = 120)
  par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

  for (res in names(resultados_def)) {
    tab <- todos_intervalos[[paste(casa, res)]]

    plot(NA, xlim = c(0, 1), ylim = c(0, 1),
         xlab = "Probabilidad de la casa (bruta)",
         ylab = "Frecuencia real observada",
         main = paste(casa, "—", res), las = 1)

    abline(0, 1, col = "gray40", lwd = 1.5, lty = 2)
    grid(col = "gray88", lty = 1)

    if (!is.null(tab) && nrow(tab) > 0) {
      lines(tab$prob_media,  tab$freq_real, col = colores[res], lwd = 2)
      points(tab$prob_media, tab$freq_real, col = colores[res], pch = 19, cex = 1.2)
      text(tab$prob_media,   tab$freq_real,
           labels = paste0("n=", tab$n), pos = 3, cex = 0.65, col = "gray30")
    }
  }

  dev.off()
  cat("\nGráfica guardada:", archivo_png)
}

# ---- 7. GUARDAR ---------------------------------------------
tabla_int <- do.call(rbind, todos_intervalos)
tabla_int <- tabla_int[, c("casa","resultado","intervalo","n",
                            "prob_media","freq_real","diferencia")]
rownames(tabla_int) <- NULL

write_excel_csv2(tabla_predicciones, file.path(carpeta, "calibracion_predicciones.csv"))
write_excel_csv2(tabla_int,          file.path(carpeta, "calibracion_intervalos.csv"))
write_excel_csv2(tabla_met,          file.path(carpeta, "metricas_calibracion.csv"))

cat("\n\nArchivos guardados en '", carpeta, "/'\n", sep = "")
cat("  - calibracion_predicciones.csv  (partido x casa: prob, prediccion, acierto)\n")
cat("  - calibracion_intervalos.csv\n")
cat("  - metricas_calibracion.csv\n")
for (casa in casas) cat(paste0("  - calibracion_", tolower(casa), ".png\n"))

# ---- 8. CSV COMPARATIVO: BRUTAS vs NORMALIZADAS -------------
# Una fila por partido x casa con probabilidades brutas,
# los 3 métodos de normalización y la suma de cada uno

norm_completo <- read_csv2(arch_norm, show_col_types = FALSE)
norm_completo <- norm_completo[norm_completo$tipo == "Casa de apuestas", ]

comparativo <- data.frame(
  Fecha              = norm_completo$Date,
  Local              = norm_completo$HomeTeam,
  Visitante          = norm_completo$AwayTeam,
  Casa               = norm_completo$casa,
  # Margen
  Margen_pct         = round(as.numeric(norm_completo$margen) * 100, 4),
  # Brutas
  PL_bruta           = round(as.numeric(norm_completo$prob_local),      4),
  PE_bruta           = round(as.numeric(norm_completo$prob_empate),     4),
  PV_bruta           = round(as.numeric(norm_completo$prob_visitante),  4),
  Suma_bruta         = round(as.numeric(norm_completo$prob_local) +
                             as.numeric(norm_completo$prob_empate) +
                             as.numeric(norm_completo$prob_visitante),  4),
  # Multiplicativo
  PL_mult            = round(as.numeric(norm_completo$PL_mult), 4),
  PE_mult            = round(as.numeric(norm_completo$PE_mult), 4),
  PV_mult            = round(as.numeric(norm_completo$PV_mult), 4),
  Suma_mult          = round(as.numeric(norm_completo$suma_mult),       4),
  # Aditivo
  PL_adit            = round(as.numeric(norm_completo$PL_adit), 4),
  PE_adit            = round(as.numeric(norm_completo$PE_adit), 4),
  PV_adit            = round(as.numeric(norm_completo$PV_adit), 4),
  Suma_adit          = round(as.numeric(norm_completo$suma_adit),       4),
  # Logarítmico
  Exponente_n        = round(as.numeric(norm_completo$exponente_n),     5),
  PL_log             = round(as.numeric(norm_completo$PL_log),  4),
  PE_log             = round(as.numeric(norm_completo$PE_log),  4),
  PV_log             = round(as.numeric(norm_completo$PV_log),  4),
  Suma_log           = round(as.numeric(norm_completo$suma_log),        4),
  stringsAsFactors   = FALSE
)

write_excel_csv2(comparativo, file.path(carpeta, "comparativo_brutas_vs_norm.csv"))
cat("  - comparativo_brutas_vs_norm.csv\n")

cat("\n— Muestra del comparativo (primeros 5 partidos, B365) —\n")
print(comparativo[comparativo$Casa == "B365", ][1:5, ], row.names = FALSE)

# ---- 9. GRÁFICA: BRUTAS vs NORMALIZADAS (por casa) ----------
# Para cada casa: 3 paneles (Local / Empate / Visitante)
# Cada panel muestra prob. bruta, multiplicativa, aditiva y logarítmica

resultados_graf <- list(
  Local     = list(Bruta = "PL_bruta", Multiplicativo = "PL_mult",
                   Aditivo = "PL_adit", Logaritmico = "PL_log"),
  Empate    = list(Bruta = "PE_bruta", Multiplicativo = "PE_mult",
                   Aditivo = "PE_adit", Logaritmico = "PE_log"),
  Visitante = list(Bruta = "PV_bruta", Multiplicativo = "PV_mult",
                   Aditivo = "PV_adit", Logaritmico = "PV_log")
)

col_metodos <- c(Bruta          = "tomato",
                 Multiplicativo = "steelblue",
                 Aditivo        = "darkorange",
                 Logaritmico    = "forestgreen")
lty_metodos <- c(Bruta = 1, Multiplicativo = 2, Aditivo = 3, Logaritmico = 1)
pch_metodos <- c(Bruta = 16, Multiplicativo = 17, Aditivo = 15, Logaritmico = 18)

for (casa in casas) {
  archivo_png <- file.path(carpeta,
                           paste0("brutas_vs_norm_", tolower(casa), ".png"))
  png(archivo_png, width = 1500, height = 520, res = 120)
  par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

  sub_comp <- comparativo[comparativo$Casa == casa, ]
  sub_norm_c <- norm[norm$casa == casa, ]

  for (res in names(resultados_graf)) {
    cols <- resultados_graf[[res]]

    # Calcular frecuencia real por intervalo para cada método
    col_real <- switch(res,
                       Local     = "real_local",
                       Empate    = "real_empate",
                       Visitante = "real_visitante")
    y <- as.numeric(sub_norm_c[[col_real]])

    plot(NA, xlim = c(0, 1), ylim = c(0, 1),
         xlab = "Probabilidad predicha",
         ylab = "Frecuencia real observada",
         main = paste(casa, "—", res), las = 1)
    abline(0, 1, col = "gray40", lwd = 1.5, lty = 2)
    grid(col = "gray88", lty = 1)

    for (met in names(cols)) {
      p_col <- cols[[met]]
      # p viene del comparativo para bruta/mult/adit/log
      p <- as.numeric(sub_comp[[p_col]])

      tab <- tabla_cal(p, y)
      if (!is.null(tab) && nrow(tab) > 0) {
        lines(tab$prob_media, tab$freq_real,
              col = unname(col_metodos[met]), lwd = 2,
              lty = unname(lty_metodos[met]))
        points(tab$prob_media, tab$freq_real,
               col = unname(col_metodos[met]),
               pch = unname(pch_metodos[met]), cex = 1.1)
      }
    }

    if (res == "Local") {
      legend("topleft", bty = "n", lwd = 2,
             lty  = lty_metodos,
             pch  = pch_metodos,
             col  = col_metodos,
             legend = names(col_metodos), cex = 0.8)
    }
  }

  dev.off()
  cat("\nGráfica brutas vs norm guardada:", archivo_png)
}

# ---- 10. PROPORCIÓN DE PREDICCIONES CORRECTAS POR CASA ------
cat("\n\n========== PROPORCIÓN DE PREDICCIONES CORRECTAS ==========\n")
cat("(Predicción = resultado con mayor probabilidad bruta)\n\n")

resumen_aciertos <- do.call(rbind, lapply(casas, function(casa) {
  sub         <- tabla_predicciones[tabla_predicciones$Casa == casa, ]
  n_total     <- nrow(sub)
  n_correctas <- sum(sub$Acierto == "\u2713", na.rm = TRUE)
  n_incorrectas <- n_total - n_correctas

  data.frame(
    Casa               = casa,
    Total_partidos     = n_total,
    Predicciones_correctas   = n_correctas,
    Predicciones_incorrectas = n_incorrectas,
    Proporcion_correctas_pct = round(100 * n_correctas   / n_total, 2),
    Proporcion_incorrectas_pct = round(100 * n_incorrectas / n_total, 2),
    stringsAsFactors   = FALSE
  )
}))

print(resumen_aciertos, row.names = FALSE)

# Gráfica de proporción de aciertos por casa
archivo_aciertos <- file.path(carpeta, "proporcion_aciertos.png")
png(archivo_aciertos, width = 900, height = 500, res = 120)
par(mar = c(5, 5, 4, 2))

barplot(
  rbind(resumen_aciertos$Proporcion_correctas_pct,
        resumen_aciertos$Proporcion_incorrectas_pct),
  names.arg = resumen_aciertos$Casa,
  col       = c("steelblue", "tomato"),
  border    = NA,
  ylim      = c(0, 110),
  ylab      = "Porcentaje de partidos (%)",
  main      = "Proporción de predicciones correctas por casa de apuestas",
  legend.text = c("Correctas", "Incorrectas"),
  args.legend = list(bty = "n", x = "topright")
)
abline(h = 50, col = "gray40", lwd = 1.5, lty = 2)

# Etiquetas encima de cada barra
for (i in seq_along(casas)) {
  text(x = (i - 0.5) * 1.2 + 0.1,
       y = resumen_aciertos$Proporcion_correctas_pct[i] + 2,
       labels = paste0(resumen_aciertos$Proporcion_correctas_pct[i], "%"),
       cex = 0.9, font = 2)
}

dev.off()
cat("\nGráfica de aciertos guardada:", archivo_aciertos, "\n")

# Guardar CSV de aciertos
write_excel_csv2(resumen_aciertos,
                 file.path(carpeta, "proporcion_aciertos.csv"))
cat("  - proporcion_aciertos.csv\n")
for (casa in casas)
  cat(paste0("  - brutas_vs_norm_", tolower(casa), ".png\n"))
