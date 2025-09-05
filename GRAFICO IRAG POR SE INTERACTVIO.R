# ----------------------------------------------------------------
# GRAFICO N°1: CASOS DE IRAG E IRAG EXTENDIDA POR SE
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# LIMPIEZA DE BASE Y NUEVO DATAFRAME
# ----------------------------------------------------------------

base_limpia<-dfnom %>%
  select(CLASIFICACION_MANUAL,SEPI_MIN_INTERNACION,ANIO_MIN_INTERNACION, EDAD_UC_IRAG)#dataframe con las variables a utilizar

base_limpia <- base_limpia %>%
  filter(CLASIFICACION_MANUAL %in% c("Infección respiratoria aguda grave (IRAG)", "IRAG extendida"))


unique(base_limpia$CLASIFICACION_MANUAL) #chequear que solo queden las clasificaciones de IRAG E IRAG EXT


# ----------------------------------------------------------------
# PASOS PARA GRAFICO CASOS DE IRAG/IRAG EXTENDIDA POR SEMANA EPIDEM
# ----------------------------------------------------------------

##PASO 1: CREAR UNA TABLA CON LOS CASOS DE IRAG E IRAG EXT POR AÑO Y SEMANA

CASOS_SEMANA_ANIO <- base_limpia %>%
  group_by(ANIO_MIN_INTERNACION, SEPI_MIN_INTERNACION, CLASIFICACION_MANUAL) %>%
  summarise(CASOS = n(), .groups = "drop")

##PASO 2: AGREGAR A LA TABLA LA SEMANA LABEL-CONVERTIR A FACTOR-PIVOTEAR

# Agrupar y crear columna de semana
CASOS_SEMANA_ANIO <- base_limpia %>%
  group_by(ANIO_MIN_INTERNACION, SEPI_MIN_INTERNACION, CLASIFICACION_MANUAL) %>%
  summarise(CASOS = n(), .groups = "drop") %>%
  mutate(SEMANA_LABEL = paste0(ANIO_MIN_INTERNACION, "-SE-", str_pad(SEPI_MIN_INTERNACION, 2, pad = "0")))

# Ordenar factor por año y semana
CASOS_SEMANA_ANIO <- CASOS_SEMANA_ANIO %>%
  arrange(ANIO_MIN_INTERNACION, SEPI_MIN_INTERNACION) %>%
  mutate(SEMANA_LABEL = factor(SEMANA_LABEL, levels = unique(SEMANA_LABEL)))

# PIVOTEAR a formato ancho
casos_pivot <- CASOS_SEMANA_ANIO %>%
  pivot_wider(
    names_from = CLASIFICACION_MANUAL,
    values_from = CASOS,
    values_fill = list(CASOS = 0)
  )

##PASO 3: ASIGNAR COLORES

colores <- c("Infección respiratoria aguda grave (IRAG)" = "#1F77B4",  # azul
             "IRAG extendida" = "#CC8052")  # naranja suave

##PASO 4: CREAR GRAFICO

##GRAFICO INTERACTIVO
GRAFICO_INTERACTIVO_SE <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "GRAFICO N°1: Casos de IRAG e IRAG extendida por semana epidemiológica",
           style = list(fontWeight = "bold", fontSize = "16px")) %>%
  hc_subtitle(text = "Desde SE 18 del 2024 hasta SE 30 del 2025. Hospital Avellaneda. Provincia de Tucumán") %>%
  hc_xAxis(
    categories = as.character(casos_pivot$SEMANA_LABEL),
    title = list(text = "Semana epidemiológica"),
    labels = list(rotation = -90, style = list(fontSize = "9px")),
    tickInterval = 1  # ← fuerza mostrar todas las semanas
  ) %>%
  hc_yAxis(title = list(text = "Número de casos")) %>%
  hc_plotOptions(column = list(
    stacking = "normal",
    borderColor = "#000000",
    borderWidth = 0.5
  )) %>%
  hc_add_series(
    name = "Infección respiratoria aguda grave (IRAG)",
    data = casos_pivot$`Infección respiratoria aguda grave (IRAG)`,
    color = "#1F77B4"
  ) %>%
  hc_add_series(
    name = "IRAG extendida",
    data = casos_pivot$`IRAG extendida`,
    color = "#CC8052"
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
  hc_exporting(enabled = TRUE) %>%
  hc_plotOptions(column = list(
    stacking = "normal",
    borderColor = "#000000",
    borderWidth = 0.5,
    pointPadding = 0,     # menos espacio entre barras individuales
    groupPadding = 0     # menos espacio entre grupos de barras
  )) %>%

print(GRAFICO_INTERACTIVO_SE)


##GRAFICO FIJO

ggplot(CASOS_SEMANA_ANIO, aes(x = SEMANA_LABEL, y = CASOS, fill = CLASIFICACION_MANUAL)) +
  geom_bar(stat = "identity", position = "stack", color = "black", size = 0.15) +  # borde fino negro para separar barras
  scale_fill_manual(values = colores) + #scale_fill_manual() para elegir colores personalizados.
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "GRAFICO N°1: Casos de IRAG e IRAG extendida por semana epidemiológica",
    subtitle = " Desde SE 18 del 2024 hasta SE 30 del 2025. Hospital Avellaneda.Provincia de Tucumán",
    x = "Semana epidemiológica",
    y = "Número de casos",
    fill = "Clasificación de caso",
    caption = 
      "Fuente: Elaboración propia en base a los datos provenientes del Sistema Nacional de Vigilancia de la Salud SNVS 2.0"
  ) +
  theme_minimal(base_family = "Arial") +  # fuente clara y común para informes
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(size = 11, hjust = 0, margin = margin(b = 15)),
    axis.text.x = element_text(angle = 90, size = 7, vjust = 0.5, hjust = 1), #Eje X con etiquetas rotadas 90° para evitar sobreposición
    axis.text.y = element_text(size = 9),
    axis.title = element_text(face = "bold", size = 11),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "bottom", #leyenda abajo
    legend.background = element_rect(fill = "transparent"),
    panel.grid.major.x = element_blank(),  # quitar líneas verticales para claridad
    panel.grid.minor = element_blank()
  )

