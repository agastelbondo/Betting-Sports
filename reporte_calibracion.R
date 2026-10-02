# ============================================================
# ENTREGABLE 3 — REPORTE DE CALIBRACIÓN AUTOGENERADO (HTML)
# Proyecto: Betting-Sports — Calibración de casas de apuestas
# Materia:  Estadística Industrial
# Universidad del Magdalena
#
# Descripción:
#   Genera un reporte HTML autogenerado con:
#   - Curvas de calibración con intervalos de confianza
#   - Tabla de desviaciones por intervalo
#   - Reglas de puntuación (Brier Score y ECE)
#   - Comparación entre métodos de remoción del margen
#
# Entradas (en resultados/):
#   calibracion/metricas_calibracion.csv
#   calibracion/calibracion_intervalos.csv
#   calibracion/proporcion_aciertos.csv
#   calibracion/calibracion_b365.png  (y bw, iw)
#   calibracion/brutas_vs_norm_b365.png (y bw, iw)
#   calibracion_nivel/resumen_calibracion_nivel.csv
#   calibracion_nivel/metricas_calibracion_nivel.csv
#   calibracion_nivel/calibracion_nivel_local.png (y empate, visitante)
#
# Salida:
#   resultados/reporte_calibracion.html
# ============================================================

# ============================================================
# INSTALACIÓN AUTOMÁTICA DE PAQUETES
# ============================================================
paquetes_necesarios <- c("readr", "dplyr", "base64enc")

for (p in paquetes_necesarios) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

library(readr)
library(dplyr)
library(base64enc)

if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

# ============================================================
# FUNCIÓN: imagen PNG → base64 para incrustar en HTML
# ============================================================
png_a_base64 <- function(ruta) {
  if (!file.exists(ruta)) return(NULL)
  raw  <- readBin(ruta, "raw", file.info(ruta)$size)
  b64  <- jsonlite::base64_enc(raw)   # necesita jsonlite
  paste0("data:image/png;base64,", b64)
}

# Si jsonlite no está disponible, se usa base::
img_tag <- function(ruta, alt = "", style = "max-width:100%;") {
  if (!file.exists(ruta)) {
    return(paste0('<p class="aviso">Imagen no encontrada: ', ruta, '</p>'))
  }
  raw <- readBin(ruta, "raw", file.info(ruta)$size)
  b64 <- paste0("data:image/png;base64,",
                paste(base64enc::base64encode(raw), collapse = ""))
  paste0('<img src="', b64, '" alt="', alt, '" style="', style, '">')
}



# ============================================================
# LEER DATOS
# ============================================================
car_cal  <- "resultados/calibracion"
car_niv  <- "resultados/calibracion_nivel"

metricas    <- read_csv2(file.path(car_cal, "metricas_calibracion.csv"),
                         show_col_types = FALSE)
intervalos  <- read_csv2(file.path(car_cal, "calibracion_intervalos.csv"),
                         show_col_types = FALSE)
aciertos    <- read_csv2(file.path(car_cal, "proporcion_aciertos.csv"),
                         show_col_types = FALSE)
resumen_niv <- read_csv2(file.path(car_niv, "resumen_calibracion_nivel.csv"),
                         show_col_types = FALSE)
metricas_niv <- read_csv2(file.path(car_niv, "metricas_calibracion_nivel.csv"),
                          show_col_types = FALSE)

casas <- c("B365", "BW", "IW")
nombres_casas <- c(B365 = "Bet365", BW = "Betway/Bwin", IW = "Interwetten")

# ============================================================
# HELPERS HTML
# ============================================================
tabla_html <- function(df, id = "") {
  enc <- paste0("<th>", names(df), "</th>", collapse = "")
  filas <- apply(df, 1, function(r) {
    celdas <- paste0("<td>", r, "</td>", collapse = "")
    paste0("<tr>", celdas, "</tr>")
  })
  paste0('<table id="', id, '" class="tabla">',
         "<thead><tr>", enc, "</tr></thead>",
         "<tbody>", paste(filas, collapse = ""), "</tbody>",
         "</table>")
}

# ============================================================
# CONSTRUIR HTML
# ============================================================

# -- Sección 3: curvas de calibración por casa ---------------
sec3 <- ""
for (casa in casas) {
  png_ruta <- file.path(car_cal, paste0("calibracion_", tolower(casa), ".png"))
  sec3 <- paste0(sec3,
    '<h3>', nombres_casas[casa], ' (', casa, ')</h3>',
    '<div class="img-wrap">',
    img_tag(png_ruta, paste("Calibración", casa)),
    '<div class="img-caption">Figura. Curva de calibración — ', nombres_casas[casa], '</div>',
    '</div>')
}

# -- Sección 4: tabla de desviaciones por intervalo ----------
# Solo B365 Local como ejemplo representativo en el HTML
# (el CSV completo está disponible)
int_b365_local <- intervalos %>%
  filter(casa == "B365", resultado == "Local") %>%
  select(Intervalo = intervalo, N = n,
         `Prob. media` = prob_media,
         `Freq. real` = freq_real,
         Diferencia = diferencia)

sec4 <- tabla_html(int_b365_local, id = "tabla-int")

# -- Sección 5: métricas Brier Score y ECE -------------------
met_fmt <- metricas %>%
  rename(Casa = casa, Resultado = resultado,
         `N partidos` = n_partidos,
         `Brier Score` = brier_score,
         ECE = ece)
sec5a <- tabla_html(met_fmt, id = "tabla-met")

# Proporción de aciertos
aci_fmt <- aciertos %>%
  rename(Casa = Casa,
         `Total` = Total_partidos,
         `Correctas` = Predicciones_correctas,
         `Incorrectas` = Predicciones_incorrectas,
         `% Correctas` = Proporcion_correctas_pct,
         `% Incorrectas` = Proporcion_incorrectas_pct)
sec5b <- tabla_html(aci_fmt, id = "tabla-aci")

# Gráfica de aciertos
sec5c <- paste0('<div class="img-wrap">',
  img_tag(file.path(car_cal, "proporcion_aciertos.png"),
          "Proporción de aciertos"),
  '<div class="img-caption">Figura. Proporción de predicciones correctas por casa de apuestas</div>',
  '</div>')

# -- Sección 6: comparación brutas vs normalizadas ------------
sec6 <- ""
for (casa in casas) {
  png_ruta <- file.path(car_cal, paste0("brutas_vs_norm_", tolower(casa), ".png"))
  sec6 <- paste0(sec6,
    '<h3>', nombres_casas[casa], '</h3>',
    '<div class="img-wrap">',
    img_tag(png_ruta, paste("Brutas vs Norm", casa)),
    '<div class="img-caption">Figura. Probabilidades brutas vs normalizadas — ',
    nombres_casas[casa], '</div>',
    '</div>')
}

# -- Sección 7: calibración por nivel de cuota ---------------
# Resumen
res_fmt <- resumen_niv %>%
  rename(Nivel = nivel,
         `BS Bruta` = bs_bruta, `ECE Bruta` = ece_bruta,
         `BS Norm. Log` = bs_norm_log, `ECE Norm. Log` = ece_norm_log,
         `Diff vs Medio (%)` = diff_vs_medio_pct)
sec7a <- tabla_html(res_fmt, id = "tabla-nivel")

# Gráficas por resultado
sec7b <- ""
for (res in c("local", "empate", "visitante")) {
  png_ruta <- file.path(car_niv, paste0("calibracion_nivel_", res, ".png"))
  sec7b <- paste0(sec7b,
    '<h4>Curvas por nivel — ', tools::toTitleCase(res), '</h4>',
    '<div class="img-wrap">',
    img_tag(png_ruta, paste("Calibración por nivel", res)),
    '<div class="img-caption">Figura. Calibración por nivel de cuota — ',
    tools::toTitleCase(res), '</div>',
    '</div>')
}

# Tabla de métricas por nivel (resumida: solo Local, todas casas)
met_niv_fmt <- metricas_niv %>%
  filter(!is.na(bs_bruta)) %>%
  rename(Casa = casa, Resultado = resultado, Nivel = nivel, N = n,
         `BS Bruta` = bs_bruta, `ECE Bruta` = ece_bruta,
         `BS Log` = bs_norm_log, `ECE Log` = ece_norm_log)
sec7c <- tabla_html(met_niv_fmt, id = "tabla-met-nivel")

# ============================================================
# ENSAMBLAR HTML COMPLETO
# ============================================================
html <- paste0('<!DOCTYPE html>
<html lang="es">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Reporte de Calibración — Casas de Apuestas</title>
  <style>
    :root {
      --azul:    #1a3a5c;
      --azul2:   #2563a8;
      --azul3:   #dbeafe;
      --gris1:   #f8fafc;
      --gris2:   #e2e8f0;
      --gris3:   #64748b;
      --verde:   #15803d;
      --rojo:    #b91c1c;
      --amarillo:#92400e;
      --texto:   #1e293b;
      --blanco:  #ffffff;
    }
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body {
      font-family: "Segoe UI", Arial, sans-serif;
      background: #f1f5f9;
      color: var(--texto);
      font-size: 15px;
      line-height: 1.6;
    }

    /* ── HEADER ── */
    .header {
      background: linear-gradient(135deg, var(--azul) 0%, var(--azul2) 100%);
      color: white;
      padding: 48px 60px 36px;
    }
    .header .tag {
      font-size: 11px;
      letter-spacing: 2px;
      text-transform: uppercase;
      color: #93c5fd;
      margin-bottom: 12px;
    }
    .header h1 {
      font-size: 30px;
      font-weight: 700;
      line-height: 1.2;
      margin-bottom: 10px;
    }
    .header .sub {
      font-size: 15px;
      color: #bfdbfe;
    }
    .header .stats {
      display: flex;
      gap: 36px;
      margin-top: 28px;
      padding-top: 24px;
      border-top: 1px solid rgba(255,255,255,0.15);
    }
    .header .stat-item { text-align: center; }
    .header .stat-num {
      font-size: 26px;
      font-weight: 700;
      color: #fff;
    }
    .header .stat-lbl {
      font-size: 12px;
      color: #93c5fd;
      margin-top: 2px;
    }

    /* ── LAYOUT ── */
    .container { max-width: 1100px; margin: 0 auto; padding: 40px 24px; }

    /* ── NAV ── */
    .toc {
      background: white;
      border-radius: 10px;
      padding: 24px 28px;
      margin-bottom: 32px;
      border: 1px solid var(--gris2);
      box-shadow: 0 1px 4px rgba(0,0,0,0.05);
    }
    .toc h2 {
      font-size: 13px;
      letter-spacing: 1.5px;
      text-transform: uppercase;
      color: var(--gris3);
      margin-bottom: 14px;
      border: none;
    }
    .toc ol {
      columns: 2;
      gap: 8px;
      padding-left: 20px;
    }
    .toc li { margin-bottom: 6px; }
    .toc a { color: var(--azul2); text-decoration: none; }
    .toc a:hover { text-decoration: underline; }

    /* ── SECCIONES ── */
    .section {
      background: white;
      border-radius: 10px;
      padding: 32px 36px;
      margin-bottom: 28px;
      border: 1px solid var(--gris2);
      box-shadow: 0 1px 4px rgba(0,0,0,0.05);
    }
    .section h2 {
      font-size: 20px;
      font-weight: 700;
      color: var(--azul);
      margin-bottom: 18px;
      padding-bottom: 10px;
      border-bottom: 2px solid var(--azul3);
      display: flex;
      align-items: center;
      gap: 10px;
    }
    .section h2 .num {
      background: var(--azul);
      color: white;
      width: 30px;
      height: 30px;
      border-radius: 50%;
      display: inline-flex;
      align-items: center;
      justify-content: center;
      font-size: 14px;
      flex-shrink: 0;
    }
    .section h3 {
      font-size: 16px;
      font-weight: 600;
      color: var(--azul2);
      margin: 22px 0 12px;
    }
    .section h4 {
      font-size: 15px;
      font-weight: 600;
      color: var(--gris3);
      margin: 16px 0 8px;
    }
    .section p { margin-bottom: 14px; color: var(--texto); }

    /* ── TABLAS ── */
    .tabla {
      border-collapse: collapse;
      width: 100%;
      margin: 16px 0 28px;
      font-size: 13px;
    }
    .tabla th {
      background: var(--azul);
      color: white;
      padding: 9px 12px;
      text-align: left;
    }
    .tabla td {
      border: 1px solid var(--gris2);
      padding: 7px 12px;
    }
    .tabla tr:nth-child(even) { background: #f8fafc; }

    /* ── IMÁGENES ── */
    .img-wrap {
      margin: 18px 0 28px;
      text-align: center;
    }
    .img-wrap img {
      max-width: 100%;
      border-radius: 8px;
      border: 1px solid var(--gris2);
      box-shadow: 0 2px 8px rgba(0,0,0,0.08);
    }
    .img-caption {
      font-size: 13px;
      color: var(--gris3);
      margin-top: 8px;
      font-style: italic;
    }

    /* ── CHIPS ── */
    .chip {
      display: inline-block;
      background: #e0ecf8;
      color: var(--azul);
      border-radius: 12px;
      padding: 2px 10px;
      font-size: 12px;
      margin: 2px;
    }

    /* ── AVISO ── */
    .aviso {
      background: #fef3c7;
      border-left: 4px solid #f59e0b;
      padding: 10px 14px;
      margin: 12px 0;
      font-size: 13px;
      border-radius: 4px;
    }

    footer {
      margin-top: 50px;
      font-size: 12px;
      color: var(--gris3);
      border-top: 1px solid var(--gris2);
      padding-top: 10px;
      text-align: center;
    }
  </style>
</head>
<body>

<!-- HEADER -->
<div class="header">
  <div class="tag">Estadística Industrial &bull; Universidad del Magdalena</div>
  <h1>Reporte de Calibración — Casas de Apuestas</h1>
  <div class="sub">Premier League 2021/22 &bull; Operadores: Bet365, Betway/Bwin, Interwetten</div>
  <div class="stats">
    <div class="stat-item">
      <div class="stat-num">377</div>
      <div class="stat-lbl">Partidos analizados</div>
    </div>
    <div class="stat-item">
      <div class="stat-num">3</div>
      <div class="stat-lbl">Casas de apuestas</div>
    </div>
    <div class="stat-item">
      <div class="stat-num">0.185</div>
      <div class="stat-lbl">Brier Score promedio</div>
    </div>
    <div class="stat-item">
      <div class="stat-num">3</div>
      <div class="stat-lbl">Métodos de normalización</div>
    </div>
  </div>
</div>

<div class="container">

<!-- TABLA DE CONTENIDO -->
<div class="toc">
  <h2>Contenido</h2>
  <ol>
    <li><a href="#s1">Introducción</a></li>
    <li><a href="#s2">Metodología</a></li>
    <li><a href="#s3">Curvas de Calibración por Casa</a></li>
    <li><a href="#s4">Tabla de Desviaciones por Intervalo</a></li>
    <li><a href="#s5">Reglas de Puntuación</a></li>
    <li><a href="#s6">Comparación entre Métodos de Remoción del Margen</a></li>
    <li><a href="#s7">¿Difiere la Calibración según el Nivel de Cuota?</a></li>
    <li><a href="#s8">Conclusiones</a></li>
  </ol>
</div>

<!-- S1: INTRODUCCIÓN -->
<div class="section" id="s1">
  <h2><span class="num">1</span>Introducción</h2>
  <p>
    Este reporte evalúa la <strong>calibración de las probabilidades</strong>
    publicadas por tres casas de apuestas —
    <strong>Bet365 (B365)</strong>, <strong>Betway/Bwin (BW)</strong> e
    <strong>Interwetten (IW)</strong> — para los <strong>377 partidos válidos</strong>
    de la Premier League 2021/22.
  </p>
  <p>
    Una casa de apuestas está <em>bien calibrada</em> si sus probabilidades implícitas
    (una vez eliminado el margen) coinciden con la frecuencia real de ocurrencia.
    Por ejemplo, si un equipo tiene probabilidad asignada 0.60 de ganar,
    debería ganar aproximadamente el 60% de las veces que se le asigna esa probabilidad.
  </p>
  <p>
    <span class="chip">Liga: Premier League</span>
    <span class="chip">Temporada: 2021/22</span>
    <span class="chip">Fuente: football-data.co.uk</span>
  </p>
</div>

<!-- S2: METODOLOGÍA -->
<div class="section" id="s2">
  <h2><span class="num">2</span>Metodología</h2>

  <h3>2.1 Probabilidad implícita bruta</h3>
  <p>
    La cuota decimal de cada resultado se convierte en probabilidad implícita mediante
    <code>P_i = 1 / Cuota_i</code>. Como la suma de estas probabilidades supera 1
    (el exceso es el margen de la casa), se aplican métodos de remoción antes de evaluar calibración.
  </p>

  <h3>2.2 Métodos de remoción del margen</h3>
  <table class="tabla">
    <thead><tr><th>Método</th><th>Fórmula</th><th>Suma total</th><th>Ventaja</th><th>Limitación</th></tr></thead>
    <tbody>
      <tr><td><strong>Multiplicativo</strong></td><td><code>P_i / (P_L + P_E + P_V)</code></td><td>= 1</td><td>Siempre positiva</td><td>Reduce todos igual</td></tr>
      <tr><td><strong>Aditivo</strong></td><td><code>P_i − (Margen / 3)</code></td><td>= 1</td><td>Intuitiva</td><td>Puede dar negativos</td></tr>
      <tr><td><strong>Logarítmico</strong></td><td><code>P_i^n / Σ P_j^n</code></td><td>= 1</td><td>No produce negativos</td><td>Más compleja</td></tr>
    </tbody>
  </table>

  <h3>2.3 Reglas de puntuación</h3>
  <p>
    <strong>Brier Score (BS):</strong> Error cuadrático medio entre la probabilidad
    predicha y el resultado real (0 o 1). Más bajo es mejor.
    <code>BS = (1/N) × Σ(p_i − y_i)²</code>
  </p>
  <p>
    <strong>ECE (Expected Calibration Error):</strong> Promedio ponderado de la diferencia
    entre probabilidad media y frecuencia real en cada intervalo de probabilidad.
    <code>ECE = Σ [(n_b / N) × |prob_media_b − freq_real_b|]</code>
  </p>
</div>

<!-- S3: CURVAS DE CALIBRACIÓN -->
<div class="section" id="s3">
  <h2><span class="num">3</span>Curvas de Calibración por Casa</h2>
  <p>
    Cada gráfica muestra la probabilidad predicha (eje X) contra la frecuencia
    real observada (eje Y) para los tres resultados posibles (Local, Empate, Visitante).
    La diagonal punteada representa calibración perfecta.
  </p>
  ', sec3, '
</div>

<!-- S4: TABLA DE DESVIACIONES -->
<div class="section" id="s4">
  <h2><span class="num">4</span>Tabla de Desviaciones por Intervalo</h2>
  <p>
    La siguiente tabla muestra, para <strong>Bet365 — resultado Local</strong>,
    la diferencia entre la probabilidad media predicha y la frecuencia real
    en cada intervalo de probabilidad. El CSV completo
    (<code>calibracion_intervalos.csv</code>) contiene todos los operadores y resultados.
  </p>
  ', sec4, '
  <p class="aviso">
    El CSV completo <code>resultados/calibracion/calibracion_intervalos.csv</code>
    contiene las tablas para B365, BW e IW en los tres resultados (Local, Empate, Visitante).
  </p>
</div>

<!-- S5: REGLAS DE PUNTUACIÓN -->
<div class="section" id="s5">
  <h2><span class="num">5</span>Reglas de Puntuación</h2>

  <h3>5.1 Brier Score y ECE por casa y resultado</h3>
  ', sec5a, '

  <h3>5.2 Proporción de predicciones correctas</h3>
  <p>
    La predicción de cada partido corresponde al resultado con mayor probabilidad bruta asignada.
  </p>
  ', sec5b,
  sec5c, '
</div>

<!-- S6: COMPARACIÓN MÉTODOS -->
<div class="section" id="s6">
  <h2><span class="num">6</span>Comparación entre Métodos de Remoción del Margen</h2>
  <p>
    Las curvas comparan las probabilidades brutas con las tres versiones normalizadas
    (Multiplicativo, Aditivo, Logarítmico) para evaluar si la remoción del margen
    mejora la calibración.
  </p>
  ', sec6, '
</div>

<!-- S7: CALIBRACIÓN POR NIVEL -->
<div class="section" id="s7">
  <h2><span class="num">7</span>¿Difiere la Calibración según el Nivel de Cuota?</h2>
  <p>
    Los partidos se clasifican en tres niveles (terciles) según la cuota de cierre:
    <strong>Baja</strong> (favoritos claros), <strong>Media</strong> y
    <strong>Alta</strong> (no favoritos).
  </p>

  <h3>Resumen de métricas por nivel</h3>
  ', sec7a, '

  <h3>Curvas de calibración por nivel de cuota</h3>
  ', sec7b, '

  <h3>Métricas detalladas por casa, resultado y nivel</h3>
  ', sec7c, '
</div>

<!-- S8: CONCLUSIONES -->
<div class="section" id="s8">
  <h2><span class="num">8</span>Conclusiones</h2>
  <p>
    Las tres casas de apuestas presentan niveles de calibración similares
    para la Premier League 2021/22, con Brier Scores en el rango 0.177–0.200
    y una proporción de predicciones correctas de aproximadamente 58.9%.
  </p>
  <p>
    La calibración varía significativamente según el nivel de cuota: los partidos
    con cuotas altas (no favoritos) tienen Brier Score más bajo (~0.128),
    mientras que los favoritos claros presentan mayor error cuadrático (~0.230).
  </p>
  <p>
    Los métodos de remoción del margen (Multiplicativo, Aditivo, Logarítmico)
    producen mejoras marginales en las métricas de calibración; las diferencias
    entre métodos son pequeñas comparadas con las diferencias entre niveles de cuota.
  </p>
</div>

</div><!-- /container -->

<footer>
  Generado automáticamente por <code>reporte_calibracion.R</code> —
  Proyecto Betting-Sports, Estadística Industrial,
  Universidad del Magdalena &bull; Premier League 2021/22
</footer>

</body>
</html>')

# ============================================================
# GUARDAR Y ABRIR
# ============================================================
dir.create("resultados", showWarnings = FALSE)
ruta_html <- "resultados/reporte_calibracion.html"
writeLines(html, ruta_html)
cat("\nReporte guardado en:", ruta_html, "\n")

browseURL(ruta_html)
