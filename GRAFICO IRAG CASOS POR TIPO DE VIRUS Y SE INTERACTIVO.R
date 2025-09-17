
# ------------------------------------------------------------------------
# GRÁFICO N°3: CASOS DE IRAG E IRAG EXTENDIDA POR TIPO DE VIRUS RESPIRATORIO 
# Y SEMANA EPIDEMIOLÓGICA
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# 1. SELECCIONAR LAS VARIABLES NECESARIAS DEL DATASET ORIGINAL 'NOTTI'
# ------------------------------------------------------------------------

data_virus <- notti %>% 
  select(SEPI_MIN_INTERNACION, ANIO_MIN_INTERNACION,
         INFLUENZA_FINAL, VSR_FINAL, COVID_19_FINAL)

# ------------------------------------------------------------------------
# 2. CREAR VARIABLE BINARIA PARA INFLUENZA DETECTABLE (1 = POSITIVO, 
# 0= NEGATIVO O SIN RESULTADO)
# ------------------------------------------------------------------------

data_influ <- data_virus %>%
  mutate(detectable_influenza = case_when(
    INFLUENZA_FINAL %in% c("Influenza A (sin subtipificar)", "Influenza A H3N2",
                           "Influenza positivo-Sin Tipo", "Influenza B (sin linaje)",
                           "Influenza A H1N1") ~ 1,
    INFLUENZA_FINAL %in% c("Negativo", "Sin resultado") ~ 0,
    TRUE ~ NA_real_  # Valores no contemplados quedan como NA
  ))

# ------------------------------------------------------------------------
# 3. CREAR VARIABLE BINARIA PARA VSR DETECTABLE (1 = POSITIVO, 
# 0= NEGATIVO O SIN RESULTADO)
# ------------------------------------------------------------------------

data_vsr <- data_influ %>%
  mutate(detectable_VSR = case_when(
    VSR_FINAL %in% c("VSR", "VSR B") ~ 1,
    VSR_FINAL %in% c("Negativo", "Sin resultado") ~ 0,
    TRUE ~ NA_real_
  ))

# ------------------------------------------------------------------------
# 4. CREAR VARIABLE BINARIA PARA COVID DETECTABLE (1 = POSITIVO, 0 = NEGATIVO 
# O SIN RESULTADO)
# ------------------------------------------------------------------------

data_COVID <- data_vsr %>%
  mutate(detectable_COVID = case_when(
    COVID_19_FINAL == "Positivo" ~ 1,
    COVID_19_FINAL %in% c("Negativo", "Sin resultado") ~ 0,
    TRUE ~ NA_real_
  ))

# ------------------------------------------------------------------------
# 5. AGRUPAR POR SEMANA EPIDEMIOLÓGICA Y AÑO PARA SUMAR LOS CASOS DETECTABLES 
# DE CADA VIRUS Y NEGATIVOS
# ------------------------------------------------------------------------

data_COVID <- data_COVID %>%
  group_by(SEPI_MIN_INTERNACION, ANIO_MIN_INTERNACION) %>%
  summarise(
    casos_influenza = sum(detectable_influenza == 1, na.rm = TRUE),
    casos_vsr = sum(detectable_VSR == 1, na.rm = TRUE),
    casos_covid = sum(detectable_COVID == 1, na.rm = TRUE),
    casos_negativos = sum(detectable_influenza == 0 & detectable_VSR == 0 & detectable_COVID == 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(ANIO_MIN_INTERNACION, SEPI_MIN_INTERNACION)

# ------------------------------------------------------------------------
# 6. PREPARAR ETIQUETAS PARA EL EJE X DEL GRÁFICO Y ORDENAR FACTOR
# ------------------------------------------------------------------------

casos_sepi_virus <- data_COVID %>%
  mutate(
    sepi_label = paste0(ANIO_MIN_INTERNACION, "-SE-", stringr::str_pad(SEPI_MIN_INTERNACION, 2, pad = "0")),
    sepi_label = factor(sepi_label, levels = unique(sepi_label))
  ) %>%
  arrange(ANIO_MIN_INTERNACION, SEPI_MIN_INTERNACION)

# ------------------------------------------------------------------------
# 7. GRÁFICO
# ------------------------------------------------------------------------

GRAFICO_INTERACTIVO_VIRUS <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(
    text = "GRÁFICO N°3: Casos de IRAG estudiados para SARS-CoV-2, Influenza y VSR según resultado",
    style = list(fontWeight = "bold", fontSize = "16px")
  ) %>%
  hc_subtitle(
    text = "Desde SE 23 del 2024 hasta SE 34 del 2025. UCIRAG Hospital Avellaneda. Provincia de Tucumán"
  ) %>%
  hc_xAxis(
    categories = as.character(casos_sepi_virus$sepi_label),
    title = list(text = "Semana epidemiológica"),
    labels = list(rotation = -90, style = list(fontSize = "9px")),
    tickInterval = 1
  ) %>%
  hc_yAxis(
    title = list(text = "Número de casos"),
    reversedStacks = FALSE   # Aquí la clave para que los negativos queden arriba
  ) %>%
  hc_plotOptions(column = list(
    stacking = "normal",
    borderColor = "#000000",
    borderWidth = 0.5,
    pointPadding = 0,
    groupPadding = 0
  )) %>%
  hc_add_series(name = "Influenza", data = casos_sepi_virus$casos_influenza, color = "red") %>%
  hc_add_series(name = "VSR", data = casos_sepi_virus$casos_vsr, color = "darkgreen") %>%
  hc_add_series(name = "COVID-19", data = casos_sepi_virus$casos_covid, color = "blue") %>%
  hc_add_series(name = "Negativos", data = casos_sepi_virus$casos_negativos, color = "#D3D3D3") %>%  # gris claro
  hc_legend(
    align = "center",
    verticalAlign = "bottom",
    layout = "horizontal"
  ) %>%
  hc_tooltip(
    shared = TRUE,
    valueSuffix = " casos"
  ) %>%
  hc_credits(
    enabled = TRUE,
    text = "Fuente: Elaboración propia en base a los datos provenientes del Sistema Nacional de Vigilancia de la Salud SNVS 2.0",
    style = list(fontSize = "10px")
  ) %>%
  hc_exporting(enabled = TRUE)

print(GRAFICO_INTERACTIVO_VIRUS)









               

