# Betting-Sports

Análisis de las cuotas de las casas de apuestas deportivas en R. El proyecto limpia los datos históricos de partidos y cuotas, calcula el margen de las casas, convierte las cuotas en probabilidades implícitas y evalúa qué tan bien calibradas están esas probabilidades frente a los resultados reales.

## Qué hace

- Filtra y limpia el conjunto de datos de partidos y cuotas.
- Calcula el **margen de mercado** (el sobreprecio que cobra la casa de apuestas).
- **Normaliza las probabilidades** implícitas para eliminar el margen.
- Analiza el mercado de **gol del equipo visitante**.
- Mide la **calibración** de las probabilidades: ¿los eventos con probabilidad X% ocurren realmente cerca del X% de las veces?
- Repite la calibración **por nivel** y genera un **reporte** con los resultados.
- Hace un **inventario de cobertura** de las variables y casas de apuestas disponibles.

## Estructura del repositorio

```
Betting-Sports/
├── MAESTRO.R                        # Script principal: ejecuta todo el flujo en orden
├── E0.csv                           # Datos de entrada (partidos y cuotas)
├── diccionario.xlsx                 # Diccionario de variables del dataset
├── filtrar_variables.R              # 1. Selección de variables
├── limpieza_datos.R                 # 2. Limpieza de datos
├── margen_mercado.R                 # 3. Margen de las casas de apuestas
├── normalizacion_probabilidades.R   # 4. Probabilidades implícitas normalizadas
├── gol_visitante.R                  # 5. Análisis de gol del visitante
├── calibracion.R                    # 6. Calibración general
├── calibracion_por_nivel.R          # 7. Calibración por nivel
├── reporte_calibracion.R            # 8. Reporte de calibración
├── inventario_cobertura.R           # 9. Inventario de cobertura
└── resultados/                      # Salidas generadas (tablas, gráficos, reportes)
```

## Requisitos

- [R](https://www.r-project.org/) 4.0 o superior
- [RStudio](https://posit.co/download/rstudio-desktop/) (recomendado, ver nota abajo)
- Paquetes de R: `dplyr`, `ggplot2`, `readxl`

`MAESTRO.R` instala automáticamente los paquetes que falten. Si prefieres hacerlo a mano:

```r
install.packages(c("dplyr", "ggplot2", "readxl"))
```

## Cómo usarlo

1. Clona el repositorio:

   ```bash
   git clone https://github.com/agastelbondo/Betting-Sports.git
   cd Betting-Sports
   ```

2. Abre `MAESTRO.R` en RStudio.

3. Ejecútalo completo (`Ctrl+Shift+S` o el botón **Source**).

El script establece la carpeta de trabajo automáticamente, crea la carpeta `resultados/` si no existe y corre los scripts en este orden:

1. `filtrar_variables.R`
2. `limpieza_datos.R`
3. `margen_mercado.R`
4. `normalizacion_probabilidades.R`
5. `gol_visitante.R`
6. `calibracion.R`
7. `calibracion_por_nivel.R`
8. `reporte_calibracion.R`
9. `inventario_cobertura.R`

Si algún script falla, el proceso se detiene e indica cuál fue. Al terminar verás el mensaje `Proceso terminado`, y las salidas estarán en `resultados/`.

> **Nota:** el cambio automático de carpeta de trabajo solo funciona dentro de RStudio. Si ejecutas desde la terminal o desde otro editor, abre R en la carpeta del proyecto o usa `setwd()` antes de correr `source("MAESTRO.R")`.

## Datos

- **`E0.csv`**: partidos con resultados y cuotas de varias casas de apuestas (el formato `E0` corresponde a la liga inglesa en el estilo de [football-data.co.uk](https://www.football-data.co.uk/)).
- **`diccionario.xlsx`**: describe cada variable del dataset.

## Resultados

Todo lo que generan los scripts (tablas, gráficos y reportes de calibración) se guarda en la carpeta `resultados/`.

## Aviso

Este proyecto tiene fines **académicos y analíticos**. No constituye asesoría de inversión ni una recomendación para apostar. Las apuestas implican riesgo de pérdida económica.
