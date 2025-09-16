# ==================================================================
# GRAFICO DE DETERMINACIONES AGRUPADAS VS GRUPOS ETARIOS PEDIATRICOS Y ADULTOS
# ==================================================================

# CARGA DE LIBRERÍAS NECESARIAS
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(lubridate)
library(highcharter)
library(plotly)


# ----------------------------------------------------------------
# CREAR UN DATAFRAME LLAMADO "BASE_LIMPIA" A PARTIR DE LA BASE INICIAL QUE 
# CONTENGA LAS VARIABLES QUE PERMITIRÁN AGRUPAR LOS RESULTADOS DE LAS DETERMINACIONES 
# DE VRS, SARS E INFLUENZA Y TRANSFORMARLAS EN VARIABLES DICOTÓMICAS PARA EL GRÁFICO
# ----------------------------------------------------------------

#   -VRS_FINAL2: AGRUPA LA VARIABLE VSR_FINAL (NOMBRE CORRECTO) CON CATEGORÍAS: 
#                "VRS" (VSR, VSR A, VSR B) Y "Negativo" (Negativo, Sin resultado)
#   -INFLUENZA_FINAL2: AGRUPA INFLUENZA_FINAL CON CATEGORÍAS: 
#                "Influenza" (tipos A/B) y "Negativo" (Negativo, Sin resultado)
#   -COVID_FINAL2: AGRUPA COVID_19_FINAL CON CATEGORÍAS: 
#                "SARS_COV_2" (Positivo) y "Negativo" (Negativo, Sin resultado)

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

# ----------------------------------------------------------------
# FILTRAR DATOS SOLO PARA EL AÑO 2025
# ----------------------------------------------------------------

base_limpia <- base_limpia %>%
  filter(ANIO_MIN_INTERNACION == 2025)


# ----------------------------------------------------------------
# VERIFICAR EL TOTAL DE CASOS POSITIVOS DE CADA DETERMINACIÓN
# ----------------------------------------------------------------

resumen_casos <- base_limpia %>%
  summarise(
    VRS_positivos = sum(VRS_FINAL2 == "VRS", na.rm = TRUE),
    Influenza_positivos = sum(INFLUENZA_FINAL2 == "Influenza", na.rm = TRUE),
    COVID_positivos = sum(COVID_FINAL2 == "SARS_COV_2", na.rm = TRUE)
  )


# ----------------------------------------------------------------
# DEFINIR EL ORDEN DE LOS GRUPOS ETARIOS EN EDAD_UC_IRAG
# ----------------------------------------------------------------

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

# ----------------------------------------------------------------
# CLASIFICAR ENTRE GRUPO PEDIÁTRICO Y ADULTO
# ----------------------------------------------------------------

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


# ==================================================================
# CREAR GRAFICO INTERACTIVO DE DETERMINACIONES EN ADULTOS
# ==================================================================

# Filtrar grupo adulto y contar casos por virus
casos_adultos <- base_limpia %>%
  filter(GRUPO_EDAD_PED_ADUL == "15 y más Años") %>%
  filter(!is.na(EDAD_UC_IRAG)) %>%
  group_by(EDAD_UC_IRAG) %>%
  summarise(
    VRS = sum(VRS_FINAL2 == "VRS", na.rm = TRUE),
    INFLUENZA = sum(INFLUENZA_FINAL2 == "Influenza", na.rm = TRUE),
    SARS_COV_2 = sum(COVID_FINAL2 == "SARS_COV_2", na.rm = TRUE),
    .groups = "drop"
  )

# Transformar a formato largo
casos_adultos_long <- casos_adultos %>%
  pivot_longer(cols = c(VRS, INFLUENZA, SARS_COV_2),
               names_to = "Virus",
               values_to = "Casos")

# Crear gráfico interactivo de barras apiladas para ADULTOS
grafico_determinaciones_adultos <- plot_ly(
  data = casos_adultos_long,
  x = ~EDAD_UC_IRAG,
  y = ~Casos,
  color = ~Virus,
  type = 'bar',
  text = ~paste("Grupo etario:", EDAD_UC_IRAG, "<br>Virus:", Virus, "<br>Casos:", Casos),
  hoverinfo = "text"
) %>%
  layout(
    barmode = 'stack',
    title = "Determinaciones positivas por virus - Grupo Adulto",
    xaxis = list(title = "Grupo etario"),
    yaxis = list(title = "Número de determinaciones positivas"),
    legend = list(title = list(text = "Virus"))
  )

# ==================================================================
# MOSTRAR LOS GRÁFICOS
# ==================================================================


# grafico_determinaciones_adultos
# print(grafico_determinaciones_adultos)
