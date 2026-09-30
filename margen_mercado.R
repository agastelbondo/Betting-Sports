# ============================================================
# MARGEN DEL MERCADO POR CASA DE APUESTAS (cuotas de cierre)
#
# Entrada: resultados/E0_limpio.csv  (generado por limpieza_datos.R)
#
# Identificacion automatica de variables:
#   XCH = cuota de cierre, victoria LOCAL
#   XCD = cuota de cierre, EMPATE
#   XCA = cuota de cierre, victoria VISITANTE
#   donde X = casa de apuestas (B365, BW, IW, PS, WH, VC, Max, Avg, ...)
#
# Formulas:
#   1) Probabilidad implicita bruta:  P = 1 / Cuota
#   2) Margen del mercado:            M = (1/CL + 1/CE + 1/CV) - 1
#
# Salidas en resultados/:
#   probabilidades_margenes_largo.csv  (partido x casa: cuotas, P brutas, M)
#   margenes_por_partido.csv           (una columna de margen por casa)
#   estadisticos_margen.csv            (media, mediana, min, max, desv. est.)
#   histograma_margenes.png / boxplot_margenes.png
# ============================================================

library(readr)

if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

# ---- 1. PARAMETROS -----------------------------------------
carpeta <- "resultados"
archivo_entrada <- file.path(carpeta, "E0_limpio.csv")

# Prefijos que NO son casas reales sino agregados del mercado
# (maximo y promedio de todas las casas). Se calculan, pero se separan.
agregados <- c("Max", "Avg")

if (!file.exists(archivo_entrada)) {
  stop("No existe ", archivo_entrada, ". Ejecuta primero limpieza_datos.R")
}
datos <- read_csv(archivo_entrada, guess_max = 10000, show_col_types = FALSE)

# ---- 2. IDENTIFICAR AUTOMATICAMENTE LAS VARIABLES ----------
# Patron: [casa] + "C" + [H | D | A] al final del nombre.
# Ejemplos: B365CH -> casa "B365", local | PSCA -> casa "PS", visitante
patron <- "^(.+)C([HDA])$"
candidatas <- grep(patron, names(datos), value = TRUE)

tabla_vars <- data.frame(
  variable  = candidatas,
  casa      = sub(patron, "\\1", candidatas),
  resultado = sub(patron, "\\2", candidatas),
  stringsAsFactors = FALSE
)

# Solo casas que tengan las TRES cuotas (H, D y A)
tiene_tres <- tapply(tabla_vars$resultado, tabla_vars$casa,
                     function(x) all(c("H", "D", "A") %in% x))
casas <- names(tiene_tres)[tiene_tres]

if (length(casas) == 0) stop("No se encontraron casas con cuotas XCH, XCD y XCA.")

cat("Casas de apuestas detectadas:", paste(casas, collapse = ", "), "\n\n")
cat("Variables usadas:\n")
print(tabla_vars[tabla_vars$casa %in% casas, ], row.names = FALSE)

# ---- 3. PROBABILIDADES BRUTAS Y MARGEN POR CASA Y PARTIDO ---
calcular_casa <- function(casa) {
  CL <- suppressWarnings(as.numeric(datos[[paste0(casa, "CH")]]))  # local
  CE <- suppressWarnings(as.numeric(datos[[paste0(casa, "CD")]]))  # empate
  CV <- suppressWarnings(as.numeric(datos[[paste0(casa, "CA")]]))  # visitante

  # Solo se calcula si las tres cuotas existen y son > 1
  valido <- is.finite(CL) & is.finite(CE) & is.finite(CV) &
            CL > 1 & CE > 1 & CV > 1

  PL <- 1 / CL      # probabilidad bruta victoria local
  PE <- 1 / CE      # probabilidad bruta empate
  PV <- 1 / CV      # probabilidad bruta victoria visitante
  M  <- (1 / CL + 1 / CE + 1 / CV) - 1   # margen del mercado

  PL[!valido] <- NA; PE[!valido] <- NA; PV[!valido] <- NA; M[!valido] <- NA

  data.frame(
    Date = datos$Date, HomeTeam = datos$HomeTeam, AwayTeam = datos$AwayTeam,
    casa = casa,
    tipo = ifelse(casa %in% agregados, "Agregado del mercado", "Casa de apuestas"),
    cuota_local = CL, cuota_empate = CE, cuota_visitante = CV,
    prob_local = PL, prob_empate = PE, prob_visitante = PV,
    suma_prob = PL + PE + PV,
    margen = M,               # en decimal (0.05 = 5 %)
    margen_pct = M * 100,     # en porcentaje
    stringsAsFactors = FALSE
  )
}

largo <- do.call(rbind, lapply(casas, calcular_casa))

# Tabla ancha: una fila por partido, una columna de margen por casa
ancho <- data.frame(Date = datos$Date, HomeTeam = datos$HomeTeam,
                    AwayTeam = datos$AwayTeam)
for (x in casas) {
  ancho[[paste0("margen_", x)]] <- largo$margen[largo$casa == x]
}

# ---- 4. ESTADISTICOS DESCRIPTIVOS DEL MARGEN ---------------
# Se expresan en puntos porcentuales (margen x 100).
estadisticos <- function(m, etiqueta, tipo) {
  m <- m[!is.na(m)] * 100
  if (length(m) == 0) {
    return(data.frame(casa = etiqueta, tipo = tipo, n_partidos = 0L,
                      promedio_pct = NA, mediana_pct = NA, minimo_pct = NA,
                      maximo_pct = NA, desv_est_pct = NA))
  }
  data.frame(casa = etiqueta, tipo = tipo, n_partidos = length(m),
             promedio_pct = mean(m), mediana_pct = median(m),
             minimo_pct = min(m), maximo_pct = max(m), desv_est_pct = sd(m))
}

tabla_est <- do.call(rbind, lapply(casas, function(x) {
  estadisticos(largo$margen[largo$casa == x], x,
               ifelse(x %in% agregados, "Agregado del mercado", "Casa de apuestas"))
}))

casas_reales <- largo[largo$tipo == "Casa de apuestas", ]
tabla_est <- rbind(
  tabla_est,
  estadisticos(casas_reales$margen, "TODAS LAS CASAS (agrupado)", "Casa de apuestas")
)
tabla_est[, 4:8] <- lapply(tabla_est[, 4:8], function(x) round(x, 3))

# ---- 5. GRAFICOS -------------------------------------------
graficar_histograma <- function() {
  m <- casas_reales$margen_pct[!is.na(casas_reales$margen_pct)]
  hist(m, breaks = 30, col = "steelblue", border = "white",
       main = "Distribucion del margen del mercado (casas de apuestas)",
       xlab = "Margen del mercado (%)", ylab = "Frecuencia")
  abline(v = mean(m),   col = "red",       lwd = 2, lty = 1)
  abline(v = median(m), col = "darkgreen", lwd = 2, lty = 2)
  legend("topright", bty = "n", lwd = 2, lty = c(1, 2),
         col = c("red", "darkgreen"), legend = c("Promedio", "Mediana"))
}

graficar_boxplot <- function() {
  datos_box <- casas_reales[!is.na(casas_reales$margen_pct), ]
  boxplot(margen_pct ~ casa, data = datos_box, col = "lightblue",
          main = "Margen del mercado por casa de apuestas",
          xlab = "Casa de apuestas", ylab = "Margen del mercado (%)")
}

png(file.path(carpeta, "histograma_margenes.png"), width = 1000, height = 700, res = 120)
graficar_histograma(); dev.off()
png(file.path(carpeta, "boxplot_margenes.png"), width = 1000, height = 700, res = 120)
graficar_boxplot(); dev.off()

graficar_histograma()   # tambien se muestran en el panel de graficos
graficar_boxplot()

# ---- 6. GUARDAR TABLAS -------------------------------------
write_excel_csv2(largo,        file.path(carpeta, "probabilidades_margenes_largo.csv"))
write_excel_csv2(ancho,        file.path(carpeta, "margenes_por_partido.csv"))
write_excel_csv2(tabla_est,    file.path(carpeta, "estadisticos_margen.csv"))

# ---- 7. VALOR DEL MERCADO (resultado explicito) ------------
cat("\n============ ESTADISTICOS DEL MARGEN (%) ============\n")
print(tabla_est, row.names = FALSE)

fila_total <- tabla_est[tabla_est$casa == "TODAS LAS CASAS (agrupado)", ]
cat(sprintf("\nVALOR DEL MERCADO: margen promedio = %.2f%% (mediana %.2f%%)\n",
            fila_total$promedio_pct, fila_total$mediana_pct))
cat("Es decir, las cuotas de cierre suman en promedio ",
    sprintf("%.2f%%", 100 + fila_total$promedio_pct),
    " de probabilidad implicita (el exceso sobre 100% es la ganancia de la casa).\n", sep = "")
cat("\nArchivos guardados en '", carpeta, "/'\n", sep = "")
