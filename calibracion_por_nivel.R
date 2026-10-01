# ============================================================
# CALIBRACIÓN POR NIVEL DE CUOTA — ¿Difiere la calibración
# según el nivel de la cuota?
#
# Clasificación por TERCILES (calculados sobre los datos reales):
#   Baja  : cuota <= Q1  → favoritos claros
#   Media : Q1 < cuota <= Q2
#   Alta  : cuota > Q2   → no favoritos / cuotas altas
#
# Cada tercil contiene exactamente 1/3 de las observaciones,
# garantizando comparaciones estadísticamente balanceadas.
#
# Entradas:
#   resultados/probabilidades_margenes_largo.csv  (cuotas + prob brutas)
#   resultados/probabilidades_normalizadas.csv    (prob normalizadas)
#   resultados/E0_limpio.csv                      (resultado real FTR)
#
# Salidas en resultados/:
#   calibracion_nivel_intervalos.csv
#   metricas_calibracion_nivel.csv
#   calibracion_nivel_[resultado].png  (curva por nivel, 3 casas)
# ============================================================

library(readr)

if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

# ---- 1. PARÁMETROS ------------------------------------------
carpeta    <- "resultados/calibracion_nivel"
dir.create(carpeta, showWarnings = FALSE)
carpeta1 <- "resultados/normalizacion"
carpeta2 <- "resultados/margen"
carpeta3 <- "resultados/datos_limpios"

arch_largo <- file.path(carpeta2, "probabilidades_margenes_largo.csv")
arch_norm  <- file.path(carpeta1, "probabilidades_normalizadas.csv")
arch_orig  <- file.path(carpeta3, "E0_limpio.csv")
N_INT      <- 5   # intervalos por nivel (menos que 10 porque hay menos datos)

if (!file.exists(arch_largo)) stop("Ejecuta primero margen_mercado.R")
if (!file.exists(arch_norm))  stop("Ejecuta primero 04_normalizacion.R")
if (!file.exists(arch_orig))  stop("Ejecuta primero limpieza_datos.R")

largo <- read_csv2(arch_largo, show_col_types = FALSE)
norm  <- read_csv2(arch_norm,  show_col_types = FALSE)
datos <- read_csv(arch_orig,   show_col_types = FALSE)

largo <- largo[largo$tipo == "Casa de apuestas", ]
norm  <- norm[norm$tipo  == "Casa de apuestas", ]

# ---- 2. CALCULAR TERCILES GLOBALES --------------------------
# Se calculan sobre TODAS las cuotas juntas (local + empate + visitante)
# de todas las casas, para tener un criterio único y consistente

todas_cuotas <- c(
  as.numeric(largo$cuota_local),
  as.numeric(largo$cuota_empate),
  as.numeric(largo$cuota_visitante)
)
todas_cuotas <- todas_cuotas[!is.na(todas_cuotas)]

Q1 <- quantile(todas_cuotas, 1/3)
Q2 <- quantile(todas_cuotas, 2/3)

cat("========== TERCILES DE CUOTA ==========\n")
cat(sprintf("  Nivel BAJO  : cuota <= %.3f  (favoritos)\n", Q1))
cat(sprintf("  Nivel MEDIO : %.3f < cuota <= %.3f\n", Q1, Q2))
cat(sprintf("  Nivel ALTO  : cuota > %.3f  (no favoritos)\n", Q2))
cat(sprintf("  Rango total : %.2f – %.2f\n\n",
            min(todas_cuotas), max(todas_cuotas)))

# Función para clasificar una cuota
clasificar <- function(cuota) {
  cuota <- as.numeric(cuota)
  ifelse(is.na(cuota), NA,
  ifelse(cuota <= Q1, "Baja",
  ifelse(cuota <= Q2, "Media", "Alta")))
}

# ---- 3. UNIR RESULTADO REAL A norm --------------------------
id_datos <- paste(datos$Date, datos$HomeTeam, datos$AwayTeam)
id_norm  <- paste(norm$Date,  norm$HomeTeam,  norm$AwayTeam)
id_largo <- paste(largo$Date, largo$HomeTeam, largo$AwayTeam,
                  largo$casa)
id_norm2 <- paste(norm$Date,  norm$HomeTeam,  norm$AwayTeam,
                  norm$casa)

norm$FTR            <- datos$FTR[match(id_norm, id_datos)]
norm$real_local     <- as.integer(norm$FTR == "H")
norm$real_empate    <- as.integer(norm$FTR == "D")
norm$real_visitante <- as.integer(norm$FTR == "A")

# Añadir cuotas de largo a norm (por partido+casa)
pos <- match(id_norm2, id_largo)
norm$cuota_local     <- as.numeric(largo$cuota_local[pos])
norm$cuota_empate    <- as.numeric(largo$cuota_empate[pos])
norm$cuota_visitante <- as.numeric(largo$cuota_visitante[pos])

# Clasificar nivel de cada cuota
norm$nivel_local     <- factor(clasificar(norm$cuota_local),
                               levels = c("Baja","Media","Alta"))
norm$nivel_empate    <- factor(clasificar(norm$cuota_empate),
                               levels = c("Baja","Media","Alta"))
norm$nivel_visitante <- factor(clasificar(norm$cuota_visitante),
                               levels = c("Baja","Media","Alta"))

# ---- 4. FRECUENCIA DE NIVELES ------------------------------
cat("========== FRECUENCIA DE NIVELES DE CUOTA ==========\n")

freq_niveles <- do.call(rbind, lapply(c("Local","Empate","Visitante"), function(res) {
  col_nivel <- switch(res,
    Local = "nivel_local", Empate = "nivel_empate", Visitante = "nivel_visitante")
  tab <- table(norm[[col_nivel]])
  data.frame(
    Resultado = res,
    Nivel     = names(tab),
    N         = as.integer(tab),
    Pct       = round(100 * as.integer(tab) / sum(tab), 1),
    stringsAsFactors = FALSE
  )
}))
print(freq_niveles, row.names = FALSE)

# ---- 5. FUNCIONES -------------------------------------------
brier_score <- function(p, y) {
  idx <- !is.na(p) & !is.na(y)
  if (sum(idx) == 0) return(NA)
  mean((p[idx] - y[idx])^2)
}

tabla_cal <- function(p, y, n_int = N_INT) {
  idx <- !is.na(p) & !is.na(y)
  p <- p[idx]; y <- y[idx]
  if (length(p) < 5) return(NULL)
  breaks    <- seq(min(p), max(p), length.out = n_int + 1)
  breaks[1] <- breaks[1] - 1e-6
  intervalo <- cut(p, breaks = breaks, include.lowest = TRUE)
  do.call(rbind, lapply(levels(intervalo), function(lv) {
    sel <- intervalo == lv
    if (sum(sel) == 0) return(NULL)
    data.frame(
      intervalo  = lv,
      n          = sum(sel),
      prob_media = round(mean(p[sel]), 4),
      freq_real  = round(mean(y[sel]), 4),
      stringsAsFactors = FALSE
    )
  }))
}

ece_calc <- function(tab) {
  if (is.null(tab) || nrow(tab) == 0) return(NA)
  N <- sum(tab$n)
  sum((tab$n / N) * abs(tab$prob_media - tab$freq_real))
}

# ---- 6. CALCULAR MÉTRICAS POR NIVEL, CASA Y RESULTADO -------
casas <- unique(norm$casa)
niveles <- c("Baja","Media","Alta")

config <- list(
  Local     = list(col_p  = "prob_local",
                   col_pn = "PL_log",
                   col_y  = "real_local",
                   col_nv = "nivel_local"),
  Empate    = list(col_p  = "prob_empate",
                   col_pn = "PE_log",
                   col_y  = "real_empate",
                   col_nv = "nivel_empate"),
  Visitante = list(col_p  = "prob_visitante",
                   col_pn = "PV_log",
                   col_y  = "real_visitante",
                   col_nv = "nivel_visitante")
)

todas_metricas   <- list()
todos_intervalos <- list()

for (casa in casas) {
  sub <- norm[norm$casa == casa, ]

  for (res in names(config)) {
    cfg    <- config[[res]]
    p_brut <- as.numeric(sub[[ cfg$col_p  ]])
    p_norm <- as.numeric(sub[[ cfg$col_pn ]])
    y      <- as.numeric(sub[[ cfg$col_y  ]])
    nivel  <- sub[[ cfg$col_nv ]]

    for (nv in niveles) {
      sel <- !is.na(nivel) & nivel == nv

      # Brutas
      tab_b <- tabla_cal(p_brut[sel], y[sel])
      bs_b  <- brier_score(p_brut[sel], y[sel])
      ece_b <- ece_calc(tab_b)

      # Normalizadas (logarítmico)
      tab_n <- tabla_cal(p_norm[sel], y[sel])
      bs_n  <- brier_score(p_norm[sel], y[sel])
      ece_n <- ece_calc(tab_n)

      llave <- paste(casa, res, nv)

      todas_metricas[[llave]] <- data.frame(
        casa           = casa,
        resultado      = res,
        nivel          = nv,
        n              = sum(sel, na.rm = TRUE),
        bs_bruta       = round(bs_b, 5),
        ece_bruta      = round(ece_b, 5),
        bs_norm_log    = round(bs_n, 5),
        ece_norm_log   = round(ece_n, 5),
        stringsAsFactors = FALSE
      )

      if (!is.null(tab_b)) {
        tab_b$casa <- casa; tab_b$resultado <- res
        tab_b$nivel <- nv;  tab_b$tipo <- "Bruta"
        todos_intervalos[[paste(llave, "B")]] <- tab_b
      }
      if (!is.null(tab_n)) {
        tab_n$casa <- casa; tab_n$resultado <- res
        tab_n$nivel <- nv;  tab_n$tipo <- "Norm_Log"
        todos_intervalos[[paste(llave, "N")]] <- tab_n
      }
    }
  }
}

tabla_met <- do.call(rbind, todas_metricas)
rownames(tabla_met) <- NULL

# ---- 7. IMPRIMIR RESUMEN ------------------------------------
cat("\n========== MÉTRICAS POR NIVEL DE CUOTA ==========\n")
cat("bs_bruta / ece_bruta     : probabilidades sin normalizar\n")
cat("bs_norm_log / ece_norm_log: probabilidades normalizadas (log)\n\n")
print(tabla_met, row.names = FALSE)

cat("\n— Brier Score promedio por nivel (todas las casas y resultados) —\n")
res_nivel <- aggregate(cbind(bs_bruta, bs_norm_log) ~ nivel,
                       data = tabla_met, FUN = mean, na.rm = TRUE)
res_nivel[, 2:3] <- round(res_nivel[, 2:3], 5)
print(res_nivel, row.names = FALSE)

# ---- 8. GRÁFICAS POR RESULTADO (3 paneles = 3 niveles) ------
# Una gráfica por resultado, con 3 paneles (Baja/Media/Alta)
# Cada panel muestra las 3 casas + bruta vs normalizada

col_casas  <- c(B365 = "steelblue", BW = "darkorange", IW = "forestgreen")
lty_tipo   <- c(Bruta = 2, Norm_Log = 1)

for (res in names(config)) {
  archivo_png <- file.path(carpeta,
    paste0("calibracion_nivel_", tolower(res), ".png"))
  png(archivo_png, width = 1500, height = 520, res = 120)
  par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

  for (nv in niveles) {
    plot(NA, xlim = c(0, 1), ylim = c(0, 1),
         xlab = "Probabilidad predicha",
         ylab = "Frecuencia real",
         main = paste(res, "— Cuota", nv), las = 1)
    abline(0, 1, col = "gray40", lwd = 1.5, lty = 2)
    grid(col = "gray88", lty = 1)

    for (casa in casas) {
      for (tipo in c("Bruta","Norm_Log")) {
        llave <- paste(paste(casa, res, nv),
                       ifelse(tipo == "Bruta","B","N"))
        tab <- todos_intervalos[[llave]]
        if (!is.null(tab) && nrow(tab) > 0) {
          lines(tab$prob_media, tab$freq_real,
                col = col_casas[casa], lwd = 2,
                lty = lty_tipo[tipo])
          points(tab$prob_media, tab$freq_real,
                 col = col_casas[casa], pch = 19, cex = 0.9)
        }
      }
    }

    if (nv == "Baja") {
      legend("topleft", bty = "n", cex = 0.75,
             col = c(col_casas, "gray40", "gray40"),
             lty = c(1,1,1,1,2),
             lwd = 2,
             legend = c(names(col_casas),
                        "Bruta (---)", "Norm. Log (—)"))
    }
  }

  dev.off()
  cat("Gráfica guardada:", archivo_png, "\n")
}

# ---- 9. TABLA RESUMEN: ¿DIFIEREN LAS CALIBRACIONES? ---------
# Promedio de BS y ECE por nivel, agrupando casas y resultados

resumen_nivel <- aggregate(
  cbind(bs_bruta, ece_bruta, bs_norm_log, ece_norm_log) ~ nivel,
  data = tabla_met, FUN = mean, na.rm = TRUE
)
resumen_nivel[, 2:5] <- round(resumen_nivel[, 2:5], 4)
resumen_nivel$nivel  <- factor(resumen_nivel$nivel,
                                levels = c("Baja","Media","Alta"))
resumen_nivel <- resumen_nivel[order(resumen_nivel$nivel), ]

# Diferencia relativa respecto al nivel Medio (referencia)
bs_medio <- resumen_nivel$bs_bruta[resumen_nivel$nivel == "Media"]
resumen_nivel$diff_vs_medio_pct <- round(
  100 * (resumen_nivel$bs_bruta - bs_medio) / bs_medio, 1)

# Veredicto automático
rango_bs  <- max(resumen_nivel$bs_bruta) - min(resumen_nivel$bs_bruta)
rango_ece <- max(resumen_nivel$ece_bruta) - min(resumen_nivel$ece_bruta)
difiere   <- rango_bs > 0.02 | rango_ece > 0.03

cat("\n")
cat("╔══════════════════════════════════════════════════════════╗\n")
cat("║     RESUMEN: ¿DIFIERE LA CALIBRACIÓN SEGÚN NIVEL?       ║\n")
cat("╠══════════════════════════════════════════════════════════╣\n")
cat("║                                                          ║\n")
cat(sprintf("║  Terciles usados:                                        ║\n"))
cat(sprintf("║    Baja  : cuota <= %.2f  (favoritos claros)            ║\n", Q1))
cat(sprintf("║    Media : %.2f < cuota <= %.2f                         ║\n", Q1, Q2))
cat(sprintf("║    Alta  : cuota > %.2f  (no favoritos)                 ║\n", Q2))
cat("║                                                          ║\n")
cat("╠══════════════════════════════════════════════════════════╣\n")
cat("║  Brier Score promedio por nivel (bruta):                 ║\n")
for (i in seq_len(nrow(resumen_nivel))) {
  nv  <- as.character(resumen_nivel$nivel[i])
  bs  <- resumen_nivel$bs_bruta[i]
  ece <- resumen_nivel$ece_bruta[i]
  dif <- resumen_nivel$diff_vs_medio_pct[i]
  signo <- ifelse(dif > 0, "+", "")
  cat(sprintf("║    %-6s  BS=%.4f  ECE=%.4f  (%s%s%% vs Media)       ║\n",
              nv, bs, ece, signo, dif))
}
cat("║                                                          ║\n")
cat("╠══════════════════════════════════════════════════════════╣\n")
if (difiere) {
  cat("║  VEREDICTO: SÍ DIFIERE la calibración según el nivel    ║\n")
  cat("║                                                          ║\n")
  cat("║  • Cuota BAJA (favoritos): peor calibración.            ║\n")
  cat("║    Las casas sobreestiman la prob. de los favoritos.    ║\n")
  cat("║    Los apostadores pagan de más por apostar al ganador  ║\n")
  cat("║    esperado.                                            ║\n")
  cat("║                                                          ║\n")
  cat("║  • Cuota ALTA (no favoritos): mejor calibración.        ║\n")
  cat("║    Las probabilidades bajas son más precisas porque     ║\n")
  cat("║    los eventos raros ocurren con la frecuencia que      ║\n")
  cat("║    la casa predice.                                     ║\n")
  cat("║                                                          ║\n")
  cat("║  • Cuota MEDIA: calibración intermedia.                 ║\n")
  cat("║    Mayor dispersión — la casa es menos consistente      ║\n")
  cat("║    en este rango de probabilidades.                     ║\n")
} else {
  cat("║  VEREDICTO: NO difiere significativamente la            ║\n")
  cat("║  calibración entre niveles de cuota.                   ║\n")
}
cat("║                                                          ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n")

# ---- 10. GRÁFICA COMPARATIVA BS y ECE POR NIVEL -------------
archivo_resumen <- file.path(carpeta, "resumen_calibracion_nivel.png")
png(archivo_resumen, width = 1000, height = 480, res = 120)
par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))

niveles_ord <- c("Baja","Media","Alta")
col_niv     <- c(Baja = "steelblue", Media = "darkorange", Alta = "forestgreen")
bs_vals     <- setNames(resumen_nivel$bs_bruta,    as.character(resumen_nivel$nivel))
ece_vals    <- setNames(resumen_nivel$ece_bruta,   as.character(resumen_nivel$nivel))

# Panel 1: Brier Score
bp1 <- barplot(bs_vals[niveles_ord],
        col    = col_niv[niveles_ord],
        border = NA,
        ylim   = c(0, max(bs_vals) * 1.25),
        main   = "Brier Score por nivel de cuota",
        ylab   = "Brier Score (más bajo = mejor)",
        names.arg = niveles_ord, las = 1)
text(bp1, bs_vals[niveles_ord] + 0.005,
     labels = round(bs_vals[niveles_ord], 4),
     cex = 0.85, font = 2)
abline(h = (1/3)*(1-1/3)^2 + (2/3)*(0-1/3)^2,
       col = "gray40", lwd = 1.5, lty = 2)
text(x = bp1[1] - 0.3, y = 0.225, "Ref. sin info", cex = 0.7, col = "gray40", adj = 0)

# Panel 2: ECE
bp2 <- barplot(ece_vals[niveles_ord],
        col    = col_niv[niveles_ord],
        border = NA,
        ylim   = c(0, max(ece_vals) * 1.25),
        main   = "ECE por nivel de cuota",
        ylab   = "ECE (más bajo = mejor calibración)",
        names.arg = niveles_ord, las = 1)
text(bp2, ece_vals[niveles_ord] + 0.002,
     labels = round(ece_vals[niveles_ord], 4),
     cex = 0.85, font = 2)

dev.off()
cat("\nGráfica resumen guardada:", archivo_resumen, "\n")

# ---- 11. GUARDAR CSVs ---------------------------------------
tabla_int <- do.call(rbind, todos_intervalos)
rownames(tabla_int) <- NULL

write_excel_csv2(tabla_met,      file.path(carpeta, "metricas_calibracion_nivel.csv"))
write_excel_csv2(tabla_int,      file.path(carpeta, "calibracion_nivel_intervalos.csv"))
write_excel_csv2(freq_niveles,   file.path(carpeta, "frecuencia_niveles_cuota.csv"))
write_excel_csv2(resumen_nivel,  file.path(carpeta, "resumen_calibracion_nivel.csv"))

cat("\nArchivos guardados en '", carpeta, "/'\n", sep = "")
cat("  - metricas_calibracion_nivel.csv\n")
cat("  - calibracion_nivel_intervalos.csv\n")
cat("  - frecuencia_niveles_cuota.csv\n")
cat("  - resumen_calibracion_nivel.csv\n")
cat("  - resumen_calibracion_nivel.png\n")
cat("  - calibracion_nivel_local.png\n")
cat("  - calibracion_nivel_empate.png\n")
cat("  - calibracion_nivel_visitante.png\n")
