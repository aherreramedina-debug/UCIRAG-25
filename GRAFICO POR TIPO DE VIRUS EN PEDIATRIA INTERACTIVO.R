# ============================================================================
# GRÁFICO DE BARRAS HORIZONTALES: CASOS POSITIVOS POR VIRUS EN GRUPO PEDIÁTRICO
# ============================================================================

# CARGA DE LIBRERÍAS
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(lubridate)
library(highcharter)
library(plotly)

# ============================================================================
# PASO 1: CREAR VARIABLES AGRUPADAS PARA VIRUS
# ============================================================================

base_limpia <- base_limpia %>%
  mutate(
    VRS_FINAL2 = case_when(
      VSR_FINAL %in% c("VSR", "VSR A", "VSR B") ~ "VRS",
      VSR_FINAL %in% c("Negativo", "Sin resultado", "Sin Resultado") ~ "Negativo",
      is.na(VSR_FINAL) ~ "Sin dato",
      TRUE ~ "Otro"
    ),
    INFLUENZA_FINAL2 = case_when(
      INFLUENZA_FINAL %in% c(
        "Influenza A (sin subtipificar)", "Influenza A H1N1", "Influenza A H3N2",
        "Influenza B (sin linaje)", "Influenza B Victoria", "Influenza B Yamagata",
        "Virus Influenza"
      ) ~ "Influenza",
      INFLUENZA_FINAL %in% c("Negativo", "Sin resultado", "Sin Resultado") ~ "Negativo",
      is.na(INFLUENZA_FINAL) ~ "Sin dato",
      TRUE ~ "Otro"
    ),
    COVID_FINAL2 = case_when(
      COVID_19_FINAL == "Positivo" ~ "SARS_COV_2",
      COVID_19_FINAL %in% c("Negativo", "Sin resultado", "Sin Resultado") ~ "Negativo",
      is.na(COVID_19_FINAL) ~ "Sin dato",
      TRUE ~ "Otro"
    )
  )

# ============================================================================
# PASO 2: FILTRAR SOLO AÑO 2025
# ============================================================================

base_limpia <- base_limpia %>%
  filter(ANIO_MIN_INTERNACION == 2025)

# ============================================================================
# PASO 3: DEFINIR ORDEN DE GRUPOS ETARIOS
# ============================================================================

orden_edades <- c(
  "0 a 2 Meses", "3 a 5 Meses", "6 a 11 Meses", "12 a 23 Meses",
  "02 a 04 Años", "05 a 09 Años", "10 a 14 Años", "15 a 19 Años",
  "20 a 24 Años", "25 a 29 Años", "30 a 34 Años", "35 a 39 Años",
  "40 a 44 Años", "45 a 49 Años", "50 a 54 Años", "55 a 59 Años",
  "60 a 64 Años", "65 a 69 Años", "70 a 74 Años", "75 y más Años"
)

base_limpia$EDAD_UC_IRAG <- factor(
  base_limpia$EDAD_UC_IRAG,
  levels = orden_edades,
  ordered = TRUE
)

# ============================================================================
# PASO 4: CLASIFICACIÓN GRUPOS PEDIÁTRICO Y ADULTO
# ============================================================================

base_limpia <- base_limpia %>%
  mutate(GRUPO_EDAD_PED_ADUL = case_when(
    EDAD_UC_IRAG %in% c(
      "0 a 2 Meses", "3 a 5 Meses", "6 a 11 Meses", "12 a 23 Meses",
      "02 a 04 Años", "05 a 09 Años", "10 a 14 Años"
    ) ~ "0 a 14 Años",
    EDAD_UC_IRAG %in% c(
      "15 a 19 Años", "20 a 24 Años", "25 a 29 Años", "30 a 34 Años",
      "35 a 39 Años", "40 a 44 Años", "45 a 49 Años", "50 a 54 Años",
      "55 a 59 Años", "60 a 64 Años", "65 a 69 Años", "70 a 74 Años", "75 y más Años"
    ) ~ "15 y más Años",
    TRUE ~ NA_character_
  ))

# ============================================================================
# PASO 5: RESUMIR CASOS POSITIVOS POR VIRUS EN GRUPO PEDIÁTRICO
# ============================================================================

casos_pediatrico <- base_limpia %>%
  filter(GRUPO_EDAD_PED_ADUL == "0 a 14 Años") %>%
  filter(!is.na(EDAD_UC_IRAG)) %>%
  group_by(EDAD_UC_IRAG) %>%
  summarise(
    VRS = sum(VRS_FINAL2 == "VRS", na.rm = TRUE),
    INFLUENZA = sum(INFLUENZA_FINAL2 == "Influenza", na.rm = TRUE),
    SARS_COV_2 = sum(COVID_FINAL2 == "SARS_COV_2", na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================================
# PASO 6: TRANSFORMAR A FORMATO LARGO PARA GRAFICAR
# ============================================================================

casos_pediatrico_long <- casos_pediatrico %>%
  pivot_longer(cols = c(VRS, INFLUENZA, SARS_COV_2),
               names_to = "Virus",
               values_to = "Casos")

# ============================================================================
# PASO 7: CREAR GRÁFICO DE BARRAS HORIZONTALES APILADAS (PLOTLY)
# ============================================================================

grafico_determinaciones_pediatria <- plot_ly(
  data = casos_pediatrico_long,
  x = ~Casos,
  y = ~EDAD_UC_IRAG,
  color = ~Virus,
  colors = c("INFLUENZA" = "red", "VRS" = "darkgreen", "SARS_COV_2" = "blue"),
  type = 'bar',
  orientation = 'h',
  text = ~paste("Grupo etario:", EDAD_UC_IRAG,
                "<br>Virus:", Virus,
                "<br>Casos:", Casos),
  hoverinfo = "text",
  textposition = "none",  # Oculta etiquetas dentro de las barras pero mantiene tooltip
  showlegend = TRUE
) %>%
  layout(
    barmode = 'stack',
    xaxis = list(
      title = "Número de casos",
      tickmode = "linear",
      dtick = 5,
      zeroline = FALSE,
      showgrid = FALSE
    ),
    yaxis = list(
      title = "",
      categoryorder = "array",
      categoryarray = rev(unique(casos_pediatrico_long$EDAD_UC_IRAG)),
      showgrid = FALSE
    ),
    plot_bgcolor = "rgba(0,0,0,0)",   # Fondo transparente sin líneas
    paper_bgcolor = "rgba(0,0,0,0)",
    legend = list(title = list(text = "Virus"))
  )

# Mostrar gráfico
grafico_determinaciones_pediatria




