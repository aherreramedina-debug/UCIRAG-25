# =====================================================
# Importación de datos base de datos UCIRAG
# =====================================================

# dfnom <- read_excel("data/Base _UC_IRAG.xlsx")

# ===============================
# 1. Frecuencias absolutas y relativas
# ===============================

frecuencias_clasif <- dfnom %>%
  count(`CLASIFICACION_MANUAL`) %>%
  mutate(prop = round(100 * n / sum(n), 1))   # porcentaje con 1 decimal

# Para acceder a cada valor individual:
frec_IRAG <- frecuencias_clasif %>% filter(`CLASIFICACION_MANUAL` == "Infección respiratoria aguda grave (IRAG)") %>% pull(n)
frec_IRAG_EXT <- frecuencias_clasif %>% filter(`CLASIFICACION_MANUAL` == "IRAG extendida") %>% pull(n)

prop_IRAG <- frecuencias_clasif %>% filter(`CLASIFICACION_MANUAL` == "Infección respiratoria aguda grave (IRAG)") %>% pull(prop)
prop_IRAG_EXT <- frecuencias_clasif %>% filter(`CLASIFICACION_MANUAL` == "IRAG extendida") %>% pull(prop)


# ===============================
# 2. Período de tiempo (columna FECHA_MINIMA)
# ===============================

dfnom$FECHA_MINIMA <- as.Date(dfnom$FECHA_MINIMA)

dfnom$FECHA_MINIMA_fmt <- format(dfnom$FECHA_MINIMA, "%d-%m-%Y")



periodo_min <- min(dfnom$FECHA_MINIMA, na.rm = TRUE)
periodo_max <- max(dfnom$FECHA_MINIMA, na.rm = TRUE)

# Para insertar directo como texto
periodo_texto <- paste0(format(periodo_min, "%d/%m/%Y"), " al ", format(periodo_max, "%d/%m/%Y"))


# ===============================
# 3. Valor único de ESTABLECIMIENTO_INTERNACION
# ===============================
establecimiento <- unique(dfnom$ESTABLECIMIENTO_INTERNACION)

unique(dfnom$ESTABLECIMIENTO_INTERNACION)


# ===============================
# 4. Total de positivos en las columnas virales
# ===============================


# Función auxiliar para el conteo de virus respi
obtener_estado_virus <- function(columna) {
  case_when(
    grepl("Influenza", columna, ignore.case = TRUE) ~ "Positivo",
    grepl("VSR", columna, ignore.case = TRUE) ~ "Positivo",
    grepl("Positivo", columna, ignore.case = TRUE) ~ "Positivo",
    TRUE ~ "Negativo"
  )
}

# Aplico la función a cada columna
dfnom <- dfnom %>%
  mutate(across(c(INFLUENZA_FINAL, VSR_FINAL, COVID_19_FINAL),
                obtener_estado_virus,
                .names = "{.col}_estado"))

# Creo una columna general "algún positivo"
dfnom <- dfnom %>%
  mutate(Positivo_general = if_else(
    INFLUENZA_FINAL_estado == "Positivo" |
      VSR_FINAL_estado == "Positivo" |
      COVID_19_FINAL_estado == "Positivo",
    "Positivo", "Negativo"
  ))

# Conteo total de positivos
total_positivos <- sum(dfnom$Positivo_general == "Positivo", na.rm = TRUE)


# ========================================================
# Calcular la media y la mediana de edad de los positivos
# ========================================================


# Calcular medidas de tendencia central y dispersión de la edad de los positivos

str(dfnom$EDAD_DIAGNOSTICO)

dfnom <- dfnom %>%
  mutate(EDAD_DIAGNOSTICO = as.numeric(EDAD_DIAGNOSTICO))

unique(dfnom$EDAD_DIAGNOSTICO)



resumen_edades <- dfnom %>%
  dplyr::filter(Positivo_general == "Positivo") %>%   
  dplyr::summarise(
    media = mean(EDAD_DIAGNOSTICO, na.rm = TRUE),
    mediana = median(EDAD_DIAGNOSTICO, na.rm = TRUE),
    desvio_estandar = sd(EDAD_DIAGNOSTICO, na.rm = TRUE),
    rango_intercuartilico = IQR(EDAD_DIAGNOSTICO, na.rm = TRUE),
    p25 = quantile(EDAD_DIAGNOSTICO, 0.25, na.rm = TRUE),
    p75 = quantile(EDAD_DIAGNOSTICO, 0.75, na.rm = TRUE),
    min = min(EDAD_DIAGNOSTICO, na.rm = TRUE),
    max = max(EDAD_DIAGNOSTICO, na.rm = TRUE)
  )



# Exploracion de los datos: tengo muchos pacientes pediatricos
class(dfnom$EDAD_DIAGNOSTICO)
str(dfnom$EDAD_DIAGNOSTICO)
head(dfnom$EDAD_DIAGNOSTICO, 100)
# ver frecuencias
dfnom %>% count(EDAD_DIAGNOSTICO) %>% arrange(desc(n)) %>% head(100)
# ver valores únicos de Positivo_general
dfnom %>% count(Positivo_general) %>% arrange(desc(n))




