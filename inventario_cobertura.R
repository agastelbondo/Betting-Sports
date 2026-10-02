# ============================================================
# ENTREGABLE 4 — INVENTARIO DE COBERTURA
# Proyecto: Betting-Sports — Calibración de casas de apuestas
# Materia:  Estadística Industrial
# Curso:    Ingeniería Industrial, Universidad del Magdalena
#
# Descripción:
#   Genera un inventario completo de los partidos disponibles
#   por operador, liga y temporada, con las exclusiones
#   aplicadas y su justificación.
#
# Entradas (todas en resultados/):
#   - datos_limpios/E0_limpio.csv
#   - datos_limpios/matriz_limpieza.csv
#   - datos_limpios/resumen_limpieza.csv
#   - datos_limpios/faltantes_por_variable.csv
#
# Salidas (en resultados/inventario/):
#   - inventario_cobertura.csv      tabla principal por operador
#   - partidos_excluidos.csv        detalle de cada partido excluido
#   - resumen_exclusiones.csv       exclusiones por regla aplicada
#   - inventario_cobertura.html     reporte legible autogenerado
# ============================================================

library(readr)
library(dplyr)
library(tidyr)
library(knitr)

# ---- Directorio de trabajo ---------------------------------
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

# ---- Parámetros externalizables ----------------------------
LIGA        <- "Premier League"
TEMPORADA   <- "2021/22"
FUENTE      <- "football-data.co.uk (E0.csv)"
PARTIDOS_TEMPORADA_COMPLETA <- 20 * 19  # 20 equipos, todos contra todos (ida y vuelta)

OPERADORES <- list(
  B365 = list(
    nombre_largo = "Bet365",
    columnas     = c("B365CH", "B365CD", "B365CA"),
    mercado      = "1X2 cierre"
  ),
  BW = list(
    nombre_largo = "Betway / Bwin",
    columnas     = c("BWCH", "BWCD", "BWCA"),
    mercado      = "1X2 cierre"
  ),
  IW = list(
    nombre_largo = "Interwetten",
    columnas     = c("IWCH", "IWCD", "IWCA"),
    mercado      = "1X2 cierre"
  )
)

# ---- Lectura de archivos -----------------------------------
cat("\n=== CARGANDO DATOS ===\n")

datos_limpios <- read_csv(
  "resultados/datos_limpios/E0_limpio.csv",
  show_col_types = FALSE
)

matriz <- read_csv2(
  "resultados/datos_limpios/matriz_limpieza.csv",
  show_col_types = FALSE
)

resumen_reglas <- read_csv2(
  "resultados/datos_limpios/resumen_limpieza.csv",
  show_col_types = FALSE
)

faltantes <- read_csv2(
  "resultados/datos_limpios/faltantes_por_variable.csv",
  show_col_types = FALSE
)

cat("Partidos en E0_limpio.csv:", nrow(datos_limpios), "\n")
cat("Filas en matriz_limpieza:", nrow(matriz), "\n")

# ============================================================
# SECCIÓN 1 — PARTIDOS EXCLUIDOS Y JUSTIFICACIÓN
# ============================================================
cat("\n=== SECCIÓN 1: EXCLUSIONES ===\n")

excluidos <- matriz %>%
  filter(conservado == 0) %>%
  select(fila_original, Date, HomeTeam, AwayTeam,
         sin_duplicado, cuotas_validas,
         medio_tiempo_ok, resultado_final_ok,
         motivo_eliminacion) %>%
  mutate(
    # Traducir motivo a descripción completa
    justificacion = case_when(
      grepl("duplicado",             motivo_eliminacion) ~
        "Registro duplicado: misma fecha, equipo local y visitante aparecen más de una vez.",
      grepl("cuota_invalida",        motivo_eliminacion) ~
        "Cuota de cierre ausente o inválida (NA / valor ≤ 1) en al menos un operador seleccionado (IW). Sin cuota completa, no es posible calcular margen ni normalizar probabilidades.",
      grepl("medio_tiempo",          motivo_eliminacion) ~
        "Inconsistencia en marcador de medio tiempo: goles al descanso superan los del resultado final.",
      grepl("resultado_final",       motivo_eliminacion) ~
        "FTR (resultado final registrado) no coincide con el resultado que se deduce de FTHG y FTAG.",
      TRUE ~ motivo_eliminacion
    ),
    liga      = LIGA,
    temporada = TEMPORADA
  ) %>%
  select(liga, temporada, fila_original, Date, HomeTeam, AwayTeam,
         sin_duplicado, cuotas_validas, medio_tiempo_ok, resultado_final_ok,
         motivo_eliminacion, justificacion)

cat("Total partidos excluidos:", nrow(excluidos), "\n")
print(excluidos %>% select(Date, HomeTeam, AwayTeam, motivo_eliminacion, justificacion))

# ============================================================
# SECCIÓN 2 — COBERTURA POR OPERADOR
# ============================================================
cat("\n=== SECCIÓN 2: COBERTURA POR OPERADOR ===\n")

inventario_operadores <- lapply(names(OPERADORES), function(op) {

  info  <- OPERADORES[[op]]
  cols  <- info$columnas

  # Cuántos de los partidos limpios tienen las tres cuotas válidas para este operador
  # (después de limpieza, IW tiene 0 faltantes en el limpio porque los 3 excluidos
  # eran precisamente los que tenían IW faltante)
  sub   <- datos_limpios %>% select(all_of(cols))
  tiene_cuota_completa <- rowSums(is.na(sub)) == 0 &
    rowSums(sub > 1, na.rm = TRUE) == 3

  n_disponibles <- sum(tiene_cuota_completa)

  # Faltantes en el CSV limpio para este operador
  falt_op <- faltantes %>%
    filter(variable %in% cols) %>%
    summarise(
      n_faltantes_limpio = sum(as.numeric(n_faltantes))
    ) %>%
    pull(n_faltantes_limpio)

  # Faltantes en el CSV ORIGINAL (antes de limpieza) para las mismas columnas
  # (tomado del resumen: los 3 excluidos eran exactamente los de IW)
  n_excluidos_por_op <- if (op == "IW") 3 else 0

  data.frame(
    liga                   = LIGA,
    temporada              = TEMPORADA,
    operador_codigo        = op,
    operador_nombre        = info$nombre_largo,
    mercado                = info$mercado,
    partidos_temporada     = PARTIDOS_TEMPORADA_COMPLETA,
    partidos_en_raw        = nrow(matriz),               # filas originales en E0
    excluidos_por_limpieza = n_excluidos_por_op,
    disponibles_para_analisis = n_disponibles,
    pct_cobertura          = round(n_disponibles / PARTIDOS_TEMPORADA_COMPLETA * 100, 2),
    faltantes_en_limpio    = falt_op,
    stringsAsFactors       = FALSE
  )
}) %>% bind_rows()

print(inventario_operadores)

# ============================================================
# SECCIÓN 3 — RESUMEN DE EXCLUSIONES POR REGLA
# ============================================================
cat("\n=== SECCIÓN 3: RESUMEN EXCLUSIONES POR REGLA ===\n")

resumen_exclusiones <- resumen_reglas %>%
  filter(regla != "TOTAL (todas las reglas)") %>%
  mutate(
    regla_descripcion = case_when(
      regla == "sin_duplicado"      ~ "Regla 1 — Sin duplicado: misma fecha y equipos no repetidos",
      regla == "cuotas_validas"     ~ "Regla 2 — Cuotas válidas: todas las cuotas de cierre son numéricas y > 1",
      regla == "medio_tiempo_ok"    ~ "Regla 3 — Consistencia medio tiempo: goles HT ≤ goles FT",
      regla == "resultado_final_ok" ~ "Regla 4 — Coherencia FTR: resultado registrado coincide con marcador",
      TRUE ~ regla
    ),
    liga      = LIGA,
    temporada = TEMPORADA
  ) %>%
  select(liga, temporada, regla, regla_descripcion,
         cumplen, incumplen, eliminados_en_secuencia, quedan_despues)

print(resumen_exclusiones)

# ============================================================
# SECCIÓN 4 — TABLA CONSOLIDADA FINAL
# ============================================================
cat("\n=== SECCIÓN 4: TABLA CONSOLIDADA ===\n")

# Nota de exclusión compartida (los 3 excluidos afectan a IW; B365 y BW
# también se excluyen en esos partidos para mantener consistencia de muestra)
nota_exclusion <- paste0(
  "Se excluyen ", nrow(excluidos), " partidos (", LIGA, " ", TEMPORADA, ") ",
  "por cuota de cierre ausente o inválida en Interwetten (IW): ",
  paste(excluidos$HomeTeam, "vs", excluidos$AwayTeam,
        paste0("(", excluidos$Date, ")"), collapse = "; "),
  ". Aunque B365 y BW tienen cobertura completa en esos 3 partidos, ",
  "se excluyen de la muestra analítica para mantener la comparabilidad ",
  "entre operadores sobre el mismo conjunto de partidos."
)

cat("\nNOTA DE EXCLUSIÓN:\n", nota_exclusion, "\n")

# ============================================================
# EXPORTACIÓN
# ============================================================
cat("\n=== EXPORTANDO ARCHIVOS ===\n")

dir.create("resultados/inventario", showWarnings = FALSE)

# 1. Inventario por operador
write_csv(
  inventario_operadores,
  "resultados/inventario/inventario_cobertura.csv"
)
cat("OK: resultados/inventario/inventario_cobertura.csv\n")

# 2. Partidos excluidos con justificación completa
write_csv(
  excluidos,
  "resultados/inventario/partidos_excluidos.csv"
)
cat("OK: resultados/inventario/partidos_excluidos.csv\n")

# 3. Resumen de reglas de exclusión
write_csv(
  resumen_exclusiones,
  "resultados/inventario/resumen_exclusiones.csv"
)
cat("OK: resultados/inventario/resumen_exclusiones.csv\n")

# ============================================================
# GENERACIÓN DEL REPORTE HTML AUTOGENERADO
# ============================================================
cat("\n=== GENERANDO REPORTE HTML ===\n")

# Función auxiliar para construir tablas HTML
tabla_html <- function(df, id = "") {
  encabezados <- paste0("<th>", names(df), "</th>", collapse = "")
  filas <- apply(df, 1, function(r) {
    celdas <- paste0("<td>", r, "</td>", collapse = "")
    paste0("<tr>", celdas, "</tr>")
  })
  paste0(
    '<table id="', id, '" class="tabla">',
    "<thead><tr>", encabezados, "</tr></thead>",
    "<tbody>", paste(filas, collapse = ""), "</tbody>",
    "</table>"
  )
}

html <- paste0('<!DOCTYPE html>
<html lang="es">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Inventario de Cobertura — Betting Sports</title>
  <style>
    body        { font-family: "Segoe UI", Arial, sans-serif; margin: 40px;
                  color: #222; background: #fafafa; }
    h1          { color: #1a3a5c; border-bottom: 3px solid #1a3a5c;
                  padding-bottom: 8px; }
    h2          { color: #2c5f8a; margin-top: 40px; }
    h3          { color: #444; }
    .tabla      { border-collapse: collapse; width: 100%; margin: 16px 0 28px; }
    .tabla th   { background: #1a3a5c; color: #fff; padding: 9px 12px;
                  text-align: left; font-size: 13px; }
    .tabla td   { border: 1px solid #ddd; padding: 8px 12px; font-size: 13px; }
    .tabla tr:nth-child(even) { background: #f0f4f8; }
    .caja-nota  { background: #fff8e1; border-left: 4px solid #f0a500;
                  padding: 12px 16px; margin: 20px 0; font-size: 13px;
                  border-radius: 4px; }
    .chip       { display: inline-block; background: #e0ecf8; color: #1a3a5c;
                  border-radius: 12px; padding: 2px 10px; font-size: 12px;
                  margin: 2px; }
    .excluido   { background: #fdecea; }
    .ok         { color: #2e7d32; font-weight: bold; }
    .warn       { color: #c62828; font-weight: bold; }
    footer      { margin-top: 50px; font-size: 12px; color: #888;
                  border-top: 1px solid #ddd; padding-top: 10px; }
  </style>
</head>
<body>

<h1>Inventario de Cobertura</h1>
<p>
  <span class="chip">Proyecto: Betting-Sports</span>
  <span class="chip">Materia: Estadística Industrial</span>
  <span class="chip">Universidad del Magdalena</span>
</p>
<p><strong>Liga:</strong> ', LIGA, ' &nbsp;|&nbsp;
   <strong>Temporada:</strong> ', TEMPORADA, ' &nbsp;|&nbsp;
   <strong>Fuente:</strong> ', FUENTE, '</p>

<!-- ======================================================= -->
<h2>1. Cobertura por operador</h2>
<p>
  Partidos disponibles para análisis una vez aplicadas todas las reglas de
  limpieza, desglosados por casa de apuestas. La muestra analítica es la misma
  para los tres operadores para garantizar comparabilidad.
</p>

',
tabla_html(
  inventario_operadores %>%
    select(
      "Liga"                    = liga,
      "Temporada"               = temporada,
      "Operador"                = operador_codigo,
      "Nombre completo"         = operador_nombre,
      "Mercado"                 = mercado,
      "Partidos (temporada)"    = partidos_temporada,
      "Partidos en raw (E0)"    = partidos_en_raw,
      "Excluidos"               = excluidos_por_limpieza,
      "Disponibles"             = disponibles_para_analisis,
      "Cobertura (%)"           = pct_cobertura
    ),
  id = "tabla-operadores"
),
'

<!-- ======================================================= -->
<h2>2. Reglas de exclusión aplicadas</h2>
<p>
  Las reglas se aplican en secuencia sobre el dataset original.
  Un partido que falla la Regla 1 no se evalúa en las siguientes.
</p>

',
tabla_html(
  resumen_exclusiones %>%
    select(
      "Regla clave"             = regla,
      "Descripción"             = regla_descripcion,
      "Cumplen"                 = cumplen,
      "Incumplen"               = incumplen,
      "Eliminados (secuencia)"  = eliminados_en_secuencia,
      "Quedan después"          = quedan_despues
    ),
  id = "tabla-reglas"
),
'

<!-- ======================================================= -->
<h2>3. Detalle de partidos excluidos</h2>
<p>
  Los <strong>', nrow(excluidos), ' partidos</strong> que no pasaron el
  control de calidad se listan a continuación con su justificación.
</p>

',
{
  # Construir tabla HTML manual para resaltar filas
  enc <- c("Fila orig.", "Fecha", "Local", "Visitante",
           "Regla fallida", "Justificación")
  enc_html <- paste0("<th>", enc, "</th>", collapse = "")
  filas_html <- apply(excluidos %>%
    select(fila_original, Date, HomeTeam, AwayTeam,
           motivo_eliminacion, justificacion), 1,
    function(r) {
      paste0(
        '<tr class="excluido">',
        "<td>", r["fila_original"], "</td>",
        "<td>", r["Date"], "</td>",
        "<td>", r["HomeTeam"], "</td>",
        "<td>", r["AwayTeam"], "</td>",
        '<td class="warn">', r["motivo_eliminacion"], "</td>",
        "<td>", r["justificacion"], "</td>",
        "</tr>"
      )
    })
  paste0(
    '<table class="tabla">',
    "<thead><tr>", enc_html, "</tr></thead>",
    "<tbody>", paste(filas_html, collapse = ""), "</tbody>",
    "</table>"
  )
},
'

<!-- ======================================================= -->
<h2>4. Nota metodológica sobre exclusiones</h2>
<div class="caja-nota">
  <strong>Criterio de consistencia entre operadores:</strong><br>
  ', nota_exclusion, '
</div>

<h3>Fuente de los datos originales</h3>
<ul>
  <li><strong>Archivo:</strong> E0.csv — descargado de
      <a href="https://www.football-data.co.uk" target="_blank">football-data.co.uk</a></li>
  <li><strong>Cobertura original:</strong> ', nrow(matriz),
    ' filas × 106 columnas (cuotas de apertura y cierre, estadísticas de partido)</li>
  <li><strong>Variables seleccionadas:</strong> 22 columnas via diccionario.xlsx
      (variables clave + cuotas de cierre de B365, BW e IW + líneas de handicap)</li>
  <li><strong>Operadores disponibles en E0.csv no seleccionados:</strong>
      Pinnacle (PS), William Hill (WH), VC Bet (VC), máximos y promedios del mercado.
      Se excluyeron porque el análisis se centra en los tres operadores con mayor
      volumen de datos completos y representatividad en el mercado europeo.</li>
</ul>

<footer>
  Generado automáticamente por <code>inventario_cobertura.R</code> —
  Proyecto Betting-Sports, Estadística Industrial,
  Universidad del Magdalena — ', TEMPORADA, '
</footer>

</body>
</html>')

writeLines(html, "resultados/inventario/inventario_cobertura.html")
cat("OK: resultados/inventario/inventario_cobertura.html\n")

# ============================================================
# RESUMEN EN CONSOLA
# ============================================================
cat("\n")
cat("============================================================\n")
cat("  INVENTARIO DE COBERTURA — RESUMEN FINAL\n")
cat("============================================================\n")
cat(sprintf("  Liga:          %s\n", LIGA))
cat(sprintf("  Temporada:     %s\n", TEMPORADA))
cat(sprintf("  Fuente:        %s\n", FUENTE))
cat(sprintf("  Partidos raw:  %d\n", nrow(matriz)))
cat(sprintf("  Excluidos:     %d (cuota IW faltante)\n", nrow(excluidos)))
cat(sprintf("  Muestra final: %d partidos\n", nrow(datos_limpios)))
cat("------------------------------------------------------------\n")
for (op in names(OPERADORES)) {
  row <- inventario_operadores %>% filter(operador_codigo == op)
  cat(sprintf("  %-5s %-18s  %d / %d  (%.1f%%)\n",
              op,
              row$operador_nombre,
              row$disponibles_para_analisis,
              row$partidos_temporada,
              row$pct_cobertura))
}
cat("============================================================\n")
cat("\nArchivos generados en resultados/inventario/:\n")
cat("  - inventario_cobertura.csv\n")
cat("  - partidos_excluidos.csv\n")
cat("  - resumen_exclusiones.csv\n")
cat("  - inventario_cobertura.html\n")

# ---- Abrir reporte en el navegador -----------------------------------------
browseURL("resultados/inventario/inventario_cobertura.html")
