# ============================================================
# PROBABILIDAD DE QUE EL VISITANTE ANOTE AL MENOS 1 GOL
#
# Entrada: resultados/E0_limpio.csv (de limpieza_datos.R)
#
# Salidas en resultados/:
#   gol_visitante_por_equipo.csv
#   gol_visitante_ranking.png
# ============================================================

library(readr)

if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

carpeta <- "resultados"
datos   <- read_csv(file.path(carpeta, "E0_limpio.csv"), show_col_types = FALSE)

# ---- PROBABILIDAD GENERAL -----------------------------------
total    <- nrow(datos)
anota    <- sum(datos$FTAG >= 1, na.rm = TRUE)
media    <- round(100 * anota / total, 2)

# ---- PROBABILIDAD POR EQUIPO --------------------------------
equipos <- sort(unique(datos$AwayTeam))

por_equipo <- do.call(rbind, lapply(equipos, function(equipo) {
  sub        <- datos[datos$AwayTeam == equipo, ]
  n_partidos <- nrow(sub)
  n_anota    <- sum(sub$FTAG >= 1, na.rm = TRUE)
  data.frame(
    Equipo          = equipo,
    Prob_anotar_pct = round(100 * n_anota / n_partidos, 2),
    stringsAsFactors = FALSE
  )
}))

por_equipo <- por_equipo[order(-por_equipo$Prob_anotar_pct), ]
por_equipo$Ranking <- seq_len(nrow(por_equipo))
por_equipo <- por_equipo[, c("Ranking", "Equipo", "Prob_anotar_pct")]
rownames(por_equipo) <- NULL

cat("========== RANKING: PROB. DE ANOTAR COMO VISITANTE ==========\n")
print(por_equipo, row.names = FALSE)

# ---- GRÁFICA ------------------------------------------------
archivo_png <- file.path(carpeta, "gol_visitante_ranking.png")
png(archivo_png, width = 1100, height = 750, res = 120)

colores <- ifelse(por_equipo$Prob_anotar_pct >= media, "steelblue", "tomato")

par(mar = c(5, 10, 4, 2))
barplot(
  rev(por_equipo$Prob_anotar_pct),
  names.arg = rev(por_equipo$Equipo),
  horiz     = TRUE,
  las       = 1,
  col       = rev(colores),
  border    = NA,
  xlab      = "Probabilidad de anotar al menos 1 gol (%)",
  main      = "Prob. de anotar >= 1 gol como visitante\nPremier League 2021/22",
  xlim      = c(0, 110)
)
abline(v = media, col = "gray30", lwd = 2, lty = 2)
text(x = media + 1, y = 0.8,
     labels = paste0("Media: ", media, "%"),
     col = "gray30", adj = 0, cex = 0.85)
legend("bottomright", bty = "n",
       fill = c("steelblue", "tomato"),
       legend = c("Sobre la media", "Bajo la media"))

dev.off()
cat("\nGráfica guardada:", archivo_png, "\n")

# ---- GUARDAR CSV --------------------------------------------
write_excel_csv2(por_equipo, file.path(carpeta, "gol_visitante_por_equipo.csv"))
cat("Archivo guardado: gol_visitante_por_equipo.csv\n")
