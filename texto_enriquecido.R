# =====================================================
# Importación de datos base de datos UCIRAG
# =====================================================

dfnom <- read_excel("data/Base _UC_IRAG.xlsx")

# =====================================================
# creacion de "base limpia" para eliminar casos invalidados
# =====================================================

base_limpia <- dfnom %>%
dplyr::filter(CLASIFICACION_MANUAL != "Caso invalidado por epidemiología")

# ===============================
# 1. Período de tiempo (columna FECHA_MINIMA)- toma los datos del año 2025 
# (columna ANIO_FECHA_MINIMA) 
# ===============================

# Asegurar que la columna de fecha esté en formato Date
base_limpia$FECHA_MINIMA <- as.Date(base_limpia$FECHA_MINIMA)

# Filtrar solo los datos del año 2025 según FECHA_MINIMA
base_2025 <- base_limpia %>%
  filter(year(FECHA_MINIMA) == 2025)

# Verificar que hay datos para 2025
if(nrow(base_2025) == 0) {
  print("No hay datos para el año 2025")
} else {
  # Obtener fechas mínima y máxima
  periodo_min <- min(base_2025$FECHA_MINIMA, na.rm = TRUE)
  periodo_max <- max(base_2025$FECHA_MINIMA, na.rm = TRUE)
  
  # Crear texto con formato de fechas
  periodo_texto <- paste0("Del ", format(periodo_min, "%d/%m/%Y"), " al ", format(periodo_max, "%d/%m/%Y"))
  
  # Mostrar texto
  print(periodo_texto)
}

# ===============================
# 2. Valor único de SE Y DE SE MINIMA Y MAXIMA
# ===============================

SE<-unique(base_limpia$SEPI_MIN_INTERNACION[base_limpia$ANIO_MIN_INTERNACION==2025])

semana_minima <- min(SE, na.rm = TRUE)
semana_maxima <- max(SE, na.rm = TRUE)


# ===============================
# 3. Valor único de ESTABLECIMIENTO_INTERNACION
# ===============================
establecimiento <- unique(na.omit(base_limpia$ESTABLECIMIENTO_INTERNACION))
establecimiento <- paste(establecimiento, collapse = ", ")

# ===============================
# 4. Frecuencias absolutas y relativas
# ===============================

# Crear tabla de frecuencias y porcentajes SOLO para el año 2025
frecuencias_clasif <- base_limpia %>%
  filter(ANIO_MIN_INTERNACION == 2025) %>%
  count(CLASIFICACION_MANUAL) %>%
  mutate(prop = round(100 * n / sum(n), 1))  # porcentaje con 1 decimal

# Frecuencias individuales
frec_IRAG <- frecuencias_clasif %>%
  filter(CLASIFICACION_MANUAL == "Infección respiratoria aguda grave (IRAG)") %>%
  pull(n)

frec_IRAG_EXT <- frecuencias_clasif %>%
  filter(CLASIFICACION_MANUAL == "IRAG extendida") %>%
  pull(n)

# Porcentajes individuales
prop_IRAG <- frecuencias_clasif %>%
  filter(CLASIFICACION_MANUAL == "Infección respiratoria aguda grave (IRAG)") %>%
  pull(prop)

prop_IRAG_EXT <- frecuencias_clasif %>%
  filter(CLASIFICACION_MANUAL == "IRAG extendida") %>%
  pull(prop)

# Total de casos (IRAG + IRAG extendida)
total_casos <- frec_IRAG + frec_IRAG_EXT

# Texto enriquecido (opcional)
texto_resumen_irag <- paste0(
  "Durante el periodo analizado (", periodo_min, " al ", periodo_max, "), ",
  "se notificaron un total de ", total_casos, " casos, de los cuales el ",
  prop_IRAG, "% (", frec_IRAG, "/", total_casos, ") correspondieron a casos de IRAG y el ",
  prop_IRAG_EXT, "% (", frec_IRAG_EXT, "/", total_casos, ") a casos de IRAG extendida."
)

# Mostrar el texto si lo deseás
print(texto_resumen_irag)


# ====================================
# 5. frecuencias y proporciones por grupos etarios
# ====================================

# Definir categorías pediátricas y adultas
categoria_pediatricos <- c(
  "0 a 2 Meses", "3 a 5 Meses", "6 a 11 Meses", "12 a 23 Meses",
  "02 a 04 Años", "05 a 09 Años", "10 a 14 Años"
)


categoria_adultos <- c(
  "15 a 19 Años", "20 a 24 Años", "25 a 29 Años", "30 a 34 Años",
  "35 a 39 Años", "40 a 44 Años", "45 a 49 Años", "50 a 54 Años",
  "55 a 59 Años", "60 a 64 Años", "65 a 69 Años", "70 a 74 Años",
  "75 y más Años"
)

# Filtrar datos de 2025 con IRAG e IRAG extendida
datos_irag_2025 <- base_limpia %>%
  filter(
    ANIO_MIN_INTERNACION == 2025,
    CLASIFICACION_MANUAL %in% c("Infección respiratoria aguda grave (IRAG)", "IRAG extendida")
  )

# Calcular frecuencias por grupo etario
frec_casos_pediatricos <- datos_irag_2025 %>%
  filter(EDAD_UC_IRAG %in% categoria_pediatricos) %>%
  tally() %>%
  pull(n)

frec_casos_adultos <- datos_irag_2025 %>%
  filter(EDAD_UC_IRAG %in% categoria_adultos) %>%
  tally() %>%
  pull(n)

# Total casos pediátricos + adultos
total_casos_ped_adult <- frec_casos_pediatricos + frec_casos_adultos

# Proporciones
prop_casos_pediatricos <- round(100 * frec_casos_pediatricos / total_casos_ped_adult, 1)
prop_casos_adultos <- round(100 * frec_casos_adultos / total_casos_ped_adult, 1)

# texto enriquecido ocpional

texto_resumen_edad <- paste0(
  "Del total de ", total_casos_ped_adult, " casos notificados de IRAG e IRAG extendida en 2025, ",
  frec_casos_pediatricos, " (", prop_casos_pediatricos, "%) correspondieron a pacientes de edad pediátrica y ",
  frec_casos_adultos, " (", prop_casos_adultos, "%) a pacientes adultos."
)

# Mostrar el texto
texto_resumen_edad


