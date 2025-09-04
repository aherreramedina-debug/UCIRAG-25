#=====================================================
# Encuentro 8
#=====================================================

install.packages(c(
  "gtable",
  "gt",
  "flextable",
  "reactable",
  "tibble",
  "tidyr"
))


# ----------------------------------------------------
# Cargar librerías
# ----------------------------------------------------

library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(readxl)
library(writexl)
library(readr)
library(gtable)
library(gt)
library(flextable)
library(reactable)
library(tibble)
library(tidyr)


# ----------------------------------------------------
# Importo los datos si no los tengo
# ----------------------------------------------------


data <- read_csv("data/VR_NOMINAL_EVENTOCASO_anonimizado.csv")




#============== Procesamiento para gráfico y tabla =======================

# OBJETIVO: Crear gráfico y tabla que muestren los casos positivos de Influenza A 
# desglosados por semana epidemiológica del evento de interés





#---- PASO Nº1 Creamos una variable para la determinación de interés ----



# Crear una variable para determinación influenza A

data_influenza <- data %>%
  mutate(detectable_influenza_a = case_when(
    # Si alguna columna tiene "Detectable", asignar 1
    `DETERMINACION_Genoma viral de Influenza A (sin subtipificar)` == "Detectable" |
      `DETERMINACION_Genoma viral de Influenza A H3N2` == "Detectable" |
      `DETERMINACION_Genoma viral de Influenza A H1N1pdm` == "Detectable" |
      `DETERMINACION_Antígeno viral de influenza A` == "Detectable" ~ 1,
    
    # Si alguna columna tiene "No detectable", asignar 0
    `DETERMINACION_Genoma viral de Influenza A (sin subtipificar)` == "No detectable" |
      `DETERMINACION_Genoma viral de Influenza A H3N2` == "No detectable" |
      `DETERMINACION_Genoma viral de Influenza A H1N1pdm` == "No detectable" |
      `DETERMINACION_Antígeno viral de influenza A` == "No detectable" ~ 0,
    
    # Si ninguna de las condiciones anteriores se cumple, asignar NA
    TRUE ~ NA
  ))


# Con |, estás diciendo explícitamente:
#   "Si alguna de las determinaciones de Influenza A es detectable → caso positivo".
# Cada columna se evalúa por separado, y si alguna devuelve TRUE, ya sumás ese caso.



# NA significa literalmente "Not Available" (no disponible).
# Es la forma que tiene R de marcar un valor faltante o desconocido dentro 
# de un vector, columna o data frame.


#------------ PASO Nº2 Construimos la variable  "semana epidemiológica" ---------


# Se crea una variable con semana epidemiológica



data_influenza <- data_influenza %>%
  mutate(semana_epidemiologica = epiweek(FECHA_APERTURA))





# ------PASO Nº3: Filtro por año, por  EVENTO y determinación---------

# Seleccionamos las variables con las que vamos a trabajar y filtro según condición

data_filtrada <- data_influenza %>% 
  filter(year(FECHA_APERTURA) == 2024) %>% 
  filter(detectable_influenza_a == 1) %>% 
  filter(EVENTO == "Internado y/o fallecido por COVID o IRA")%>% 
  select(semana_epidemiologica,EVENTO,detectable_influenza_a)


# data_filtrada_2 <- data_influenza %>% 
#   filter(
#     year(FECHA_APERTURA) %in% c(2023, 2024),
#     detectable_influenza_a == 1,
#     EVENTO %in% c(
#       "Internado y/o fallecido por COVID o IRA",
#       "COVID-19, Influenza y OVR en ambulatorios (No UMAs)"
#     )
#   ) %>% 
#   select(semana_epidemiologica, EVENTO, detectable_influenza_a)


#------------ filtros con dos años y dos eventos ---------------



# Agrupo según semana epidemiológica los casos detectables del Evento a elección

data_filtrada <- data_filtrada %>% 
  group_by(semana_epidemiologica) %>%
  summarise(casos_semana = n(), .groups = "drop") %>% 
  arrange(semana_epidemiologica)




#============== Tablas ============================



#------------- Tabla de casos por semana epidemiológica -----------------------------


# Para el armado de la tabla podemos sumar una fila de totales al data frame


data_tabla <- data_filtrada %>%
  mutate(semana_epidemiologica = as.character(semana_epidemiologica)) %>%
  bind_rows(
    data.frame(
      semana_epidemiologica = "Total",
      casos_semana = sum(data_filtrada$casos_semana, na.rm = TRUE)
    )
  )




# ----- Tabla 1  de casos por semana epidemiológica



# Tabla simple
tabla_semana <- data_tabla %>% 
  gt()

tabla_semana



# Tabla editada con parametros de visualización
tabla_semana <- data_tabla %>% 
  gt() %>%
  
  # Títulos y subtítulos
  tab_header(
    title = md("**Casos Influenza A**"),  # Título en negrita
    subtitle = md("Casos *confirmados* de influenza por semana epidemiológica")  # Subtítulo en cursiva
  ) %>%
  
  # Personalizar nombres de las columnas
  cols_label(
    semana_epidemiologica = md("Semana Epidemiológica"), 
    casos_semana = md("Casos de Influenza A")  
  ) %>%
  
  # Alineación de las cabeceras de las columnas
  opt_align_table_header(align = "left") %>%
  
  # Nota al pie de la tabla
  tab_source_note(
    source_note = md("Fuente de datos: Sistema Nacional de Vigilancia de la Salud")
  ) %>%
  
  # Agregar una nota al pie para las columnas de tasas
  tab_footnote(
    footnote = "casos notificados en el SNVS",
    locations = cells_column_labels(columns = semana_epidemiologica)  # Aplica solo a las cabeceras de columnas de tasas
  ) %>%
  
  # Aplicar colores a las celdas de los casos, utilizando una paleta de colores
  data_color(
    columns = casos_semana,  # Seleccionamos las columnas a colorear
    colors = scales::col_numeric(  # Usamos col_numeric para aplicar una paleta
      palette = c("#f7fcf5", "#bd0026"),  # Color desde un amarillo claro hasta un rojo
      domain = c(0, 5)  # Establecemos el rango de valores de la paleta
    )) %>% 
  
  # Establecer la fuente a Helvetica y evitar la negrita en todo el texto
  opt_table_font(
    font = "Helvetica", 
    weight = "normal"
  ) %>%
  
  # Alineación de las cabeceras de las columnas: columnas 1 y 2 alineadas a la izquierda
  tab_style(
    style = cell_text(align = "left"), 
    locations = cells_column_labels(columns = c(1, 2))  # Localidad, Casos_Dengue
  ) %>%
  
  # Quitar color de la fila de "Totales"
  tab_style(
    style = cell_fill(color = "gray"),  # Establecemos el color de fondo blanco (sin color)
    locations = cells_body(
      rows = nrow(data_filtrada),  # Seleccionamos la última fila (fila de totales)
      columns = casos_semana  # Aplicamos solo a la columna cantidad_evento
    )
  )


tabla_semana



# ======== Gráfico  ================================

#-------------- gráfico por semana epidemiológica ------------------------------



# Crear un dataframe con todas las semanas del año (suponiendo que tienes un año completo)
semanas_completas <- data.frame(semana_epidemiologica = 1:52)


semanas_completas <- semanas_completas %>%
  left_join(data_filtrada, by = "semana_epidemiologica") %>%
  mutate(casos_semana = ifelse(is.na(casos_semana), 0, casos_semana))



# Qué hace: Une (join) dos tablas (semanas_completas y data_filtrada) usando
# como clave la columna semana_epidemiologica.
#
# Tipo de join: left_join significa "mantener todas las filas de la tabla de la
# izquierda (semanas_completas) y traer de la derecha (data_filtrada) solo las
# coincidencias".
#
# Si en data_filtrada no existe un valor de semana_epidemiologica que esté en
# semanas_completas, en esas filas las columnas nuevas se llenan con NA.



# Si casos_semana es NA (porque no hubo datos para esa semana en data_filtrada), poner 0.
# Si no es NA, dejar el valor original.



# Crear el gráfico de líneas
grafico_semanas <- ggplot(semanas_completas,
                          aes(x = semana_epidemiologica, y = casos_semana)) +
  geom_line( color = "red", size = 1) +  # Graficar líneas Color rojo y línea más gruesa para mejor visibilidad
  labs(title = "Determinaciones de Influenza por Semana Epidemiológica. 2024", x = "Semana Epidemiológica", y = "Cantidad de Casos") +
  scale_x_continuous(breaks = 1:52) +
  theme_minimal()# Aseguramos que se muestren todas las semanas del 1 al 52

# Mostrar el gráfico
grafico_semanas






#========================= ANEXO ANÁLISIS POR DEPARTAMENTO  ===========================================
# ---------- Procesamiento y analisis de datos por Departamento de residencia -----------------------




# ------ PASO 1 ANEXO ----------------------------------
# Crear variable para determinación influenza B

data_anexo <- data_influenza %>%
  mutate(detectable_influenza_b = case_when(
    #    Si alguna columna tiene "Detectable", asignar 1
    `DETERMINACION_Genoma viral de Influenza B, linaje Victoria` == "Detectable" |
      `DETERMINACION_Antígeno viral de influenza B` == "Detectable" |
      `DETERMINACION_Genoma viral de Influenza B (sin linaje)` == "Detectable" |
      `DETERMINACION_Genoma viral de VSR B` == "Detectable" ~ 1,
    
    `DETERMINACION_Genoma viral de Influenza B, linaje Victoria` == "No detectable" |
      `DETERMINACION_Antígeno viral de influenza B` == "No detectable" |
      `DETERMINACION_Genoma viral de Influenza B (sin linaje)` == "No detectable" |
      `DETERMINACION_Genoma viral de VSR B` == "No detectable" ~ 0,
    
    #    Si ninguna de las condiciones anteriores se cumple, asignar 0
    TRUE ~ NA
  ))





# ------------PASO 2 ANEXO-------------------------



data_anexo <- data_anexo %>% 
  filter(year(FECHA_APERTURA) == 2024) %>% 
  filter(EVENTO == "Internado y/o fallecido por COVID o IRA")




# Agrupo por departamento de residencia y sumo los casos detectables para esta condición

data_casos_anexo <- data_anexo %>%
  # Filtrar para Influenza A y B por separado
  group_by(DEPARTAMENTO_RESIDENCIA) %>%
  summarise(
    cantidad_influenza_a = sum(detectable_influenza_a == 1, na.rm = TRUE),  # Contar casos de Influenza A
    cantidad_influenza_b = sum(detectable_influenza_b == 1, na.rm = TRUE),  # Contar casos de Influenza B
    .groups = "drop"
  )



# detectable_influenza_a == 1 → para cada fila, devuelve TRUE si el valor es 1,
# FALSE si no, y NA si está vacío.

# sum(...) → en R, TRUE se cuenta como 1 y FALSE como 0, así que el resultado es
# la cantidad de veces que apareció un 1.

# na.rm = TRUE → ignora los NA para que no afecten la suma.


#------------- Tabla simple anexo -----------------------------



tabla_departamento <- data_casos_anexo %>% 
  reactable() 
tabla_departamento





#------------- Tabla editada anexo -----------------------------




tabla_departamento <- reactable(
  data_casos_anexo,  # Primero pasa el dataframe
  columns = list(
    DEPARTAMENTO_RESIDENCIA = colDef(
      name = "Departamento", 
      align = "left",  # Alineación a la izquierda
      style = list(fontWeight = "bold")  # Hacer el nombre del departamento en negrita
    ),
    cantidad_influenza_a = colDef(
      name = "Casos de Influenza A",
      align = "center",  # Alineación centrada
      format = colFormat(separators = TRUE, digits = 0),  # Formato con separadores y sin decimales
      style = list(color = "#F5744A")  # Colorear los números de influenza A en rojo
    ), 
    cantidad_influenza_b = colDef(
      name = "Casos de Influenza B",
      align = "center",  # Alineación centrada
      format = colFormat(separators = TRUE, digits = 0),  # Formato con separadores y sin decimales
      style = list(color = "#5F94F5")  # Colorear los números de influenza B en azul
    )
  ),
  highlight = TRUE,  # Resalta las filas al pasar el ratón
  striped = TRUE,    # Alterna colores en las filas
  bordered = TRUE,   # Agrega bordes
  compact = TRUE,    # Usa un formato compacto para más visualización
  pagination = TRUE, # Habilita la paginación
  defaultPageSize = 10,  # Páginas de 10 elementos
  theme = reactableTheme(
    borderColor = "#CCCCCC",   # Bordes de color gris claro
    backgroundColor = "#F9F9F9", # Color de fondo muy suave
    headerStyle = list(
      backgroundColor = "#3D37F0", 
      color = "white", 
      fontWeight = "bold", 
      fontFamily = "Helvetica"
    ),  # Fuente Helvetica para los encabezados de columnas
    rowStyle = list(
      borderBottom = "1px solid #CCCCCC", 
      fontFamily = "Helvetica"
    )  # Fuente Helvetica para las filas
  )
)

# Mostrar la tabla
tabla_departamento





#-------------- gráfico de barras anexo -----------------------------------



# Reestructurar los datos a formato largo
data_long <- data_casos_anexo %>%
  pivot_longer(cols = starts_with("cantidad_influenza"),  # Selecciona las columnas de los casos de influenza
               names_to = "tipo_influenza",                # Nuevo nombre para la columna de tipo de influenza
               values_to = "cantidad",                     # Nuevo nombre para la columna de los valores
               names_prefix = "cantidad_")                 # Elimina el prefijo "cantidad_" de las columnas


# el pivoteo con pivot_longer() transforma tu tabla de formato ancho a formato
# largo:

# Antes: cada tipo de influenza está en una columna distinta
# (cantidad_influenza_a, cantidad_influenza_b).

# Después: hay una sola columna tipo_influenza que indica si es A o B, y otra
# columna cantidad que guarda el número de casos.




# Crear gráfico de barras apiladas
grafico_barras_apiladas <- ggplot(data_long, aes(x = DEPARTAMENTO_RESIDENCIA, y = cantidad, fill = tipo_influenza)) +
  geom_bar(stat = "identity", position = "stack") +  
  labs(title = "Casos de Influenza A y B por Departamento", 
       x = "Departamento", 
       y = "Cantidad de Casos", 
       fill = "Tipo de Influenza") +
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar las etiquetas del eje x para mayor legibilidad

# Mostrar el gráfico
grafico_barras_apiladas









################# COMO FUNCIONA EPIWEEK ###########################

# 1. Seleccionar algunas columnas para no abrumar
data_ejemplo <- data_influenza %>%
  select(FECHA_APERTURA) %>%
  slice(1:20)  # solo 20 filas para que sea más manejable

# 2. Agregar semana y año epidemiológico
data_ejemplo <- data_ejemplo %>%
  mutate(
    semana_epi = epiweek(FECHA_APERTURA),
    anio_epi   = epiyear(FECHA_APERTURA),
    dia_semana = wday(FECHA_APERTURA, label = TRUE, week_start = 7)
  )

###################################################################


