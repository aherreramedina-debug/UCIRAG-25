# ------------------------------------------------------------------------
# GRAFICOS DE VIRUS RESPIRATORIOS POR GRUPO DE EDAD 
# ------------------------------------------------------------------------

# ----------------------------------------------------------------
# 1. DEFINIR EL ORDEN DE LOS GRUPOS ETARIOS
# ----------------------------------------------------------------

orden_edades <- c(
  "0 a 2 Meses", "3 a 5 Meses", "6 a 11 Meses", "12 a 23 Meses",
  "02 a 04 Años", "05 a 09 Años", "10 a 14 Años", "15 a 19 Años",
  "20 a 24 Años", "25 a 29 Años", "30 a 34 Años", "35 a 39 Años",
  "40 a 44 Años", "45 a 49 Años", "50 a 54 Años", "55 a 59 Años",
  "60 a 64 Años", "65 a 69 Años", "70 a 74 Años", "75 y más Años"
)

# ----------------------------------------------------------------
# 2. FILTRAR DATOS DEL AÑO 2025 Y CREAR VARIABLES BINARIAS
# ----------------------------------------------------------------

data_clasificada <- notti %>%
  filter(ANIO_MIN_INTERNACION == 2025) %>%
  select(EDAD_UC_IRAG, INFLUENZA_FINAL, VSR_FINAL, COVID_19_FINAL) %>%
  mutate(
    # Ordenar grupos de edad
    EDAD_UC_IRAG = factor(EDAD_UC_IRAG, levels = orden_edades, ordered = TRUE),
    
    # Clasificación pediátrico vs adulto
    GRUPO_EDAD_PED_ADUL = case_when(
      EDAD_UC_IRAG %in% c("0 a 2 Meses", "3 a 5 Meses", "6 a 11 Meses",
                          "12 a 23 Meses", "02 a 04 Años", "05 a 09 Años",
                          "10 a 14 Años") ~ "0 a 14 Años",
      EDAD_UC_IRAG %in% c("15 a 19 Años", "20 a 24 Años", "25 a 29 Años", 
                          "30 a 34 Años", "35 a 39 Años", "40 a 44 Años",
                          "45 a 49 Años", "50 a 54 Años", "55 a 59 Años",
                          "60 a 64 Años", "65 a 69 Años", "70 a 74 Años",
                          "75 y más Años") ~ "15 y más Años",
      TRUE ~ NA_character_
    ),
    
    # Influenza detectable
    detectable_influenza = case_when(
      INFLUENZA_FINAL %in% c("Influenza A (sin subtipificar)", "Influenza A H3N2",
                             "Influenza positivo-Sin Tipo", "Influenza B (sin linaje)",
                             "Influenza A H1N1") ~ 1,
      INFLUENZA_FINAL %in% c("Negativo", "Sin resultado") ~ 0,
      TRUE ~ NA_real_
    ),
    
    # VSR detectable
    detectable_VSR = case_when(
      VSR_FINAL %in% c("VSR", "VSR B") ~ 1,
      VSR_FINAL %in% c("Negativo", "Sin resultado") ~ 0,
      TRUE ~ NA_real_
    ),
    
    # COVID-19 detectable
    detectable_COVID = case_when(
      COVID_19_FINAL == "Positivo" ~ 1,
      COVID_19_FINAL %in% c("Negativo", "Sin resultado") ~ 0,
      TRUE ~ NA_real_
    )
  )

# ----------------------------------------------------------------
# 3. FUNCION PARA GENERAR GRÁFICO INTERACTIVO POR GRUPO DE EDAD
# ----------------------------------------------------------------

generar_grafico_por_grupo <- function(data, titulo, subtitulo) {
  
  casos <- data %>%
    group_by(EDAD_UC_IRAG) %>%
    summarise(
      casos_influenza = sum(detectable_influenza == 1, na.rm = TRUE),
      casos_vsr = sum(detectable_VSR == 1, na.rm = TRUE),
      casos_covid = sum(detectable_COVID == 1, na.rm = TRUE),
      casos_negativos = sum(detectable_influenza == 0 & detectable_VSR == 0 & detectable_COVID == 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(EDAD_UC_IRAG)
  
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_title(text = titulo, style = list(fontWeight = "bold", fontSize = "16px")) %>%
    hc_subtitle(text = subtitulo) %>%
    hc_xAxis(
      categories = as.character(casos$EDAD_UC_IRAG),
      title = list(text = "Grupo de Edad"),
      labels = list(rotation = -45, style = list(fontSize = "10px"))
    ) %>%
    hc_yAxis(
      title = list(text = "Número de casos"),
      reversedStacks = FALSE
    ) %>%
    hc_plotOptions(column = list(
      stacking = "normal",
      borderColor = "#000000",
      borderWidth = 0.5,
      pointPadding = 0,
      groupPadding = 0
    )) %>%
    hc_add_series(name = "Influenza", data = casos$casos_influenza, color = "red") %>%
    hc_add_series(name = "VSR", data = casos$casos_vsr, color = "darkgreen") %>%
    hc_add_series(name = "COVID-19", data = casos$casos_covid, color = "blue") %>%
    hc_add_series(name = "Negativos", data = casos$casos_negativos, color = "#D3D3D3") %>%
    hc_legend(
      align = "center",
      verticalAlign = "bottom",
      layout = "horizontal"
    ) %>%
    hc_tooltip(shared = TRUE, valueSuffix = " casos") %>%
    hc_credits(
      enabled = TRUE,
      text = "Fuente: Elaboración propia en base a los datos del SNVS 2.0",
      style = list(fontSize = "10px")
    ) %>%
    hc_exporting(enabled = TRUE)
}

# ----------------------------------------------------------------
# 4. GRAFICO ADULTOS (15 AÑOS Y MÁS)
# ----------------------------------------------------------------

grafico_adultos <- data_clasificada %>%
  filter(GRUPO_EDAD_PED_ADUL == "15 y más Años") %>%
  generar_grafico_por_grupo(
    titulo = "GRÁFICO N°5: Casos totales de IRAG en adultos. Distribución de agentes virales identificados según grupo de edad",
    subtitulo = "Año 2025 - UCIRAG Hospital Avellaneda.Provincia de Tucumán"
  )

# ----------------------------------------------------------------
# 5. MOSTRAR LOS GRÁFICOS
# ----------------------------------------------------------------

print(grafico_adultos)