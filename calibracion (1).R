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
carpeta      <- "resultados"
arch_norm    <- file.path(carpeta, "probabilidades_normalizadas.csv")
arch_orig    <- file.path(carpeta, "E0_limpio.csv")
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
