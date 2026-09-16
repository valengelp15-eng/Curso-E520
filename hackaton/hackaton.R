library(tidyverse)

# 1. Limpieza y estandarización de la variable de capital
datos_capital <- balances %>% 
  mutate(
    capital_limpio = as.numeric(str_replace_all(capital_informado, ",", "."))
  ) %>% 
  filter(!is.na(capital_limpio), capital_limpio > 0)

# 2. Análisis A: Capital Promedio por Tipo Societario
resumen_promedio <- datos_capital %>% 
  group_by(descripcion_tipo_societario) %>% 
  summarize(capital_promedio = mean(capital_limpio, na.rm = TRUE)) %>% 
  mutate(porcentaje = (capital_promedio / sum(capital_promedio)) * 100) %>% 
  filter(porcentaje > 0.1) %>% 
  arrange(desc(porcentaje))

grafico_promedio <- ggplot(resumen_promedio, aes(x = reorder(descripcion_tipo_societario, -porcentaje), y = porcentaje)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = scales::comma(round(capital_promedio, 0))), vjust = -0.5, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
  labs(title = "Capital Social Promedio", x = NULL, y = "Participación (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

# 3. Análisis B: Masa Total de Capital Acumulado por Tipo Societario
resumen_total <- datos_capital %>% 
  group_by(descripcion_tipo_societario) %>% 
  summarize(capital_total = sum(capital_limpio, na.rm = TRUE)) %>% 
  mutate(porcentaje = (capital_total / sum(capital_total)) * 100) %>% 
  filter(porcentaje > 0.1) %>% 
  arrange(desc(porcentaje))

grafico_total <- ggplot(resumen_total, aes(x = reorder(descripcion_tipo_societario, -porcentaje), y = porcentaje)) +
  geom_col(fill = "darkorange") +
  geom_text(aes(label = scales::comma(round(capital_total, 0))), vjust = -0.5, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
  labs(title = "Masa Total de Capital Acumulado", x = NULL, y = "Participación (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

# Ejecutá estos comandos para visualizar cada gráfico por separado en la pestaña Plots
print(grafico_promedio)
print(grafico_total)
library(tidyverse)

# 1. Calculamos los datos de masa total y los porcentajes
resumen_total <- balances %>% 
  mutate(
    capital_limpio = as.numeric(str_replace_all(capital_informado, ",", "."))
  ) %>% 
  filter(!is.na(capital_limpio), capital_limpio > 0) %>% 
  group_by(descripcion_tipo_societario) %>% 
  summarize(capital_total = sum(capital_limpio, na.rm = TRUE)) %>% 
  mutate(
    porcentaje = (capital_total / sum(capital_total)) * 100,
    etiqueta = paste0(round(porcentaje, 1), "%")
  ) %>% 
  filter(porcentaje > 0.1) %>% 
  arrange(desc(porcentaje))

# 2. Gráfico de torta (pie chart) con ggplot2
ggplot(resumen_total, aes(x = "", y = porcentaje, fill = descripcion_tipo_societario)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(
    title = "Participación porcentual en la masa total de capital",
    fill = "Tipo Societario"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    legend.position = "right"
  )
colnames(balances)
library(tidyverse)

# 1. Procesar datos extrayendo el año de la fecha de balance
datos_evolucion <- balances %>% 
  mutate(
    capital_limpio = as.numeric(str_replace_all(capital_informado, ",", ".")),
    # Extraemos los primeros 4 caracteres correspondientes al año (ajustá si el formato es distinto)
    anio = substr(fecha_balance, 1, 4) 
  ) %>% 
  filter(!is.na(capital_limpio), capital_limpio > 0, !is.na(anio), anio >= "2015") %>% 
  group_by(anio, descripcion_tipo_societario) %>% 
  summarize(capital_total = sum(capital_limpio, na.rm = TRUE), .groups = "drop")

# 2. Gráfico de líneas para ver la evolución temporal
ggplot(datos_evolucion, aes(x = anio, y = capital_total, color = descripcion_tipo_societario, group = descripcion_tipo_societario)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Evolución histórica de la masa total de capital por tipo societario",
    x = "Año del Balance",
    y = "Capital Total Acumulado",
    color = "Tipo Societario"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
library(tidyverse)

# 1. Limpieza y filtrado de valores extremos o ceros para que la escala sea legible
datos_boxplot <- balances %>% 
  mutate(
    capital_limpio = as.numeric(str_replace_all(capital_informado, ",", "."))
  ) %>% 
  filter(!is.na(capital_limpio), capital_limpio > 0) %>% 
  group_by(descripcion_tipo_societario) %>% 
  filter(n() > 10) %>% # Filtramos categorías con muy pocos registros para evitar ruido estadístico
  ungroup()

# 2. Gráfico de cajas (Boxplot) para ver la mediana y la dispersión del capital por tipo
ggplot(datos_boxplot, aes(x = reorder(descripcion_tipo_societario, capital_limpio, FUN = median), y = capital_limpio)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.size = 1, outlier.alpha = 0.5) +
  scale_y_log10(labels = scales::comma) + # Escala logarítmica para comparar asimetrías
  labs(
    title = "Distribución y dispersión del capital por tipo societario",
    subtitle = "Mediana y valores atípicos (escala logarítmica)",
    x = "Tipo Societario",
    y = "Capital Informado (Escala Log)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
  )
library(tidyverse)

# 1. Calculamos los datos y armamos una nueva etiqueta para la leyenda
resumen_total <- balances %>% 
  mutate(
    capital_limpio = as.numeric(str_replace_all(capital_informado, ",", "."))
  ) %>% 
  filter(!is.na(capital_limpio), capital_limpio > 0) %>% 
  group_by(descripcion_tipo_societario) %>% 
  summarize(capital_total = sum(capital_limpio, na.rm = TRUE)) %>% 
  mutate(
    porcentaje = (capital_total / sum(capital_total)) * 100,
    # Esta línea fusiona el nombre con el porcentaje (ej: "SOCIEDAD EXTRANJERA (30.5%)")
    leyenda_porcentaje = paste0(descripcion_tipo_societario, " (", round(porcentaje, 1), "%)")
  ) %>% 
  filter(porcentaje > 0.1) %>% 
  arrange(desc(porcentaje))

# 2. Gráfico de torta final
ggplot(resumen_total, aes(x = "", y = porcentaje, fill = reorder(leyenda_porcentaje, -porcentaje))) +
  geom_col(width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(
    title = "Participación en la masa total de capital",
    fill = "Tipo Societario"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "right",
    legend.text = element_text(size = 10)
  )
# 2. Gráfico de torta final (Tonos azules Google Drive)
ggplot(resumen_total, aes(x = "", y = porcentaje, fill = reorder(leyenda_porcentaje, -porcentaje))) +
  geom_col(width = 1, color = "white") +
  coord_polar("y", start = 0) +
  # Esta línea inyecta la paleta de colores personalizada
  scale_fill_manual(values = c("#1A73E8", "#4285F4", "#8AB4F8", "#AECBFA", "#D2E3FC", "#174EA6", "#1967D2", "#E8F0FE")) +
  theme_void() +
  labs(
    title = "Participación en la masa total de capital",
    fill = "Tipo Societario"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "right",
    legend.text = element_text(size = 10)
  )
# Fecha del balance más antiguo
min(balances$fecha_balance, na.rm = TRUE)

# Fecha del balance más reciente
max(balances$fecha_balance, na.rm = TRUE)