# ----------------------------------------------------------------
# GRAFICO N°2: CASOS DE IRAG E IRAG EXTENDIDA POR GRUPO ETARIO
# ----------------------------------------------------------------

# CREAR UNA BASE CON LAS VARIABLES A TRABAJAR Y QUE NO CONTENGA LOS INVALIDADOS
# DEBO CREAR UN NUEVO OBJETO PARA ORDENAR LOS GRUPOS DE EDAD
# DEL NUEVO DATAFRAME LAS VARIABLES A USAR SERAN:
# CLASIFICACION MANUAL DE CASO
# EDAD_UC_IRAG


# ----------------------------------------------------------------
# ORDENAR LA VARIABLE GRUPO ETARIO y CREAR UN VECTOR PARA GRUPO DE EDAD
# ----------------------------------------------------------------

orden_edades <- c(
  "0 a 2 Meses", "3 a 5 Meses", "6 a 11 Meses", "12 a 23 Meses",
  "02 a 04 Años", "05 a 09 Años", "10 a 14 Años", "15 a 19 Años",
  "20 a 24 Años", "25 a 29 Años", "30 a 34 Años", "35 a 39 Años",
  "40 a 44 Años", "45 a 49 Años", "50 a 54 Años", "55 a 59 Años",
  "60 a 64 Años", "65 a 69 Años", "70 a 74 Años", "75 y más Años"
)

DATA_NUEVO$EDAD_UC_IRAG <- factor(
  base_limpia$EDAD_UC_IRAG,
  levels = orden_edades,
  ordered = TRUE)

valores_ordenados <- levels(DATA_NUEVO$EDAD_UC_IRAG)
valores_ordenados

# ----------------------------------------------------------------
# AGRUPAR DATOS PARA EL GRAFICO Y ELIMINAR NA
# ----------------------------------------------------------------

casos_por_edad <- DATA_NUEVO %>%
  filter(!is.na(EDAD_UC_IRAG), !is.na(CLASIFICACION_MANUAL)) %>%
  group_by(EDAD_UC_IRAG, CLASIFICACION_MANUAL) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = CLASIFICACION_MANUAL, values_from = n, values_fill = 0)

# ----------------------------------------------------------------
# GRAFICO DE IRAG TOTALES POR GRUPO ETARIO INTERACTIVO
# ----------------------------------------------------------------


GRAFICO_INTERACTIVO_EDAD <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "GRAFICO N°2: Casos de IRAG e IRAG extendida por grupo de edad",
           style = list(fontWeight = "bold", fontSize = "16px")) %>%
  hc_subtitle(text = "Desde SE 18 del 2024 hasta SE 30 del 2025. UCIRAG Hospital Avellaneda. Provincia de Tucumán") %>%
  hc_xAxis(
    categories = as.character(casos_por_edad$EDAD_UC_IRAG),
    title = list(text = "Grupo etario"),
    labels = list(rotation = -45, style = list(fontSize = "9px")),
    tickInterval = 1
  ) %>%
  hc_yAxis(title = list(text = "Número de casos")) %>%
  hc_plotOptions(column = list(
    stacking = "normal",
    borderColor = "#000000",
    borderWidth = 0.5,
    pointPadding = 0,
    groupPadding = 0
  )) %>%
  hc_add_series(
    name = "Infección respiratoria aguda grave (IRAG)",
    data = casos_por_edad$`Infección respiratoria aguda grave (IRAG)`,
    color = "#1F77B4"
  ) %>%
  hc_add_series(
    name = "IRAG extendida",
    data = casos_por_edad$`IRAG extendida`,
    color = "#33A02C"
  ) %>%
  hc_legend(
    align = "center",
    verticalAlign = "bottom",
    layout = "horizontal"
  ) %>%
  hc_tooltip(shared = TRUE, valueSuffix = " casos") %>%
  hc_credits(enabled = TRUE,
             text = "Fuente: Elaboración propia en base a los datos provenientes del Sistema Nacional de Vigilancia de la Salud SNVS 2.0",
             style = list(fontSize = "10px")) %>%
  hc_exporting(enabled = TRUE)

# Mostrar gráfico
print(GRAFICO_INTERACTIVO_EDAD)



