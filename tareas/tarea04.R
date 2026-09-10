# Cargar las librerias que usamos en la materia
library(tidyverse)
library(nycflights13)
# Vuelos con retraso de llegada de 2 horas o mas
flights %>% 
  filter(arr_delay >= 120)

# Vuelos a Houston
flights %>% 
  filter(dest %in% c("IAH", "HOU"))

# Ordenar por mayores retrasos de salida
flights %>% 
  arrange(desc(dep_delay))
# Vuelos de United, American o Delta (UA, AA, DL)
flights %>% 
  filter(carrier %in% c("UA", "AA", "DL"))

# Vuelos de verano (julio, agosto, septiembre)
flights %>% 
  filter(month %in% c(7, 8, 9))

# Llegaron mas de 2 horas tarde pero salieron a tiempo o antes
flights %>% 
  filter(arr_delay > 120 & dep_delay <= 0)

# Salieron con mas de 1 hora de demora pero recuperaron mas de 30 min en vuelo
flights %>% 
  filter(dep_delay >= 60, (dep_delay - arr_delay) > 30)

# Salieron entre la medianoche y las 6 am
flights %>% 
  filter(dep_time == 2400 | dep_time <= 600)

# Vuelos que salieron mas adelantados
flights %>% 
  arrange(dep_delay)

# Vuelos mas rapidos (calculo velocidad en mph)
flights %>% 
  mutate(speed = distance / air_time * 60) %>% 
  arrange(desc(speed)) %>% 
  select(flight, carrier, origin, dest, distance, air_time, speed)

# Mas distancia recorrida
flights %>% 
  arrange(desc(distance)) %>% 
  select(flight, carrier, origin, dest, distance)

# Menos distancia recorrida
flights %>% 
  arrange(distance) %>% 
  select(flight, carrier, origin, dest, distance)

# Conversión de HHMM a minutos para comparar tiempos
flights %>% 
  mutate(
    dep_time_min = (dep_time %/% 100) * 60 + (dep_time %% 100),
    arr_time_min = (arr_time %/% 100) * 60 + (arr_time %% 100),
    air_time_calc = arr_time_min - dep_time_min
  ) %>% 
  select(dep_time, arr_time, air_time, air_time_calc)

# Promedio de retrasos por aerolinea
flights %>% 
  group_by(carrier) %>% 
  summarize(
    avg_dep_delay = mean(dep_delay, na.rm = TRUE),
    avg_arr_delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  ) %>% 
  arrange(desc(avg_arr_delay))
# ==========================================
# PARTE 2: Claves (19.2.4 Exercises: Keys)
# ==========================================

# 1. Armar una primary key única para flights usando row_number()
flights_with_id <- flights %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())

head(flights_with_id)

# 2. Verificación de claves primarias en las demás tablas

# En planes:
planes %>% 
  count(tailnum) %>% 
  filter(n > 1)

# En airlines:
airlines %>% 
  count(carrier) %>% 
  filter(n > 1)

# En airports:
airports %>% 
  count(faa) %>% 
  filter(n > 1)

# En weather:
weather %>% 
  count(origin, year, month, day, hour) %>% 
  filter(n > 1)
# ==========================================
# PARTE 3: Uniones (19.3.4 Exercises: Joins)
# ==========================================

# 1. Promedio de retrasos por destino y mapa con airports
avg_delay_dest <- flights %>% 
  group_by(dest) %>% 
  summarize(avg_arr_delay = mean(arr_delay, na.rm = TRUE))

dest_delays_geo <- avg_delay_dest %>% 
  inner_join(airports, by = c("dest" = "faa"))

ggplot(dest_delays_geo, aes(x = lon, y = lat, color = avg_arr_delay)) +
  borders("state") +
  geom_point(aes(size = avg_arr_delay), alpha = 0.7) +
  coord_quickmap() +
  scale_color_viridis_c() +
  labs(
    title = "Retrasos promedio de llegada segun el aeropuerto de destino",
    x = "Longitud",
    y = "Latitud",
    color = "Retraso (min)",
    size = "Retraso (min)"
  ) +
  theme_minimal()

# 2. Agregar coordenadas de origen y destino a flights
flights_with_loc <- flights %>% 
  left_join(airports %>% select(faa, lat, lon), by = c("origin" = "faa")) %>% 
  rename(origin_lat = lat, origin_lon = lon) %>% 
  left_join(airports %>% select(faa, lat, lon), by = c("dest" = "faa")) %>% 
  rename(dest_lat = lat, dest_lon = lon)

flights_with_loc %>% 
  select(flight, origin, origin_lat, origin_lon, dest, dest_lat, dest_lon)

# 3. Relacion entre la antigüedad del avion y los retrasos
plane_ages <- planes %>% 
  mutate(plane_age = 2013 - year) %>% 
  select(tailnum, plane_age)

age_delay <- flights %>% 
  inner_join(plane_ages, by = "tailnum") %>% 
  group_by(plane_age) %>% 
  summarize(
    avg_arr_delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  ) %>% 
  filter(plane_age < 50)

ggplot(age_delay, aes(x = plane_age, y = avg_arr_delay)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(
    title = "Relación entre la antigüedad del avion y la demora al llegar",
    x = "Antigüedad del avión (años)",
    y = "Retraso promedio de llegada (min)"
  ) +
  theme_minimal()

# 4. Impacto de la visibilidad en demoras (flights + weather)
flights_weather <- flights %>% 
  inner_join(weather, by = c("origin", "year", "month", "day", "hour"))

flights_weather %>% 
  group_by(visib) %>% 
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  ggplot(aes(x = visib, y = avg_dep_delay)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Efecto de la visibilidad en el retraso de salida",
    x = "Visibilidad (millas)",
    y = "Retraso promedio de salida (min)"
  ) +
  theme_minimal()

# 5. Analisis del 13 de junio de 2013
june13 <- flights %>% 
  filter(year == 2013, month == 6, day == 13) %>% 
  group_by(hour) %>% 
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE))

ggplot(june13, aes(x = hour, y = avg_dep_delay)) +
  geom_line(color = "firebrick", size = 1) +
  geom_point() +
  labs(
    title = "Evolución de retrasos el 13 de Junio de 2013",
    x = "Hora del dia",
    y = "Retraso promedio (min)"
  ) +
  theme_minimal()

# Clima ese mismo dia (precipitación y viento)
weather %>% 
  filter(year == 2013, month == 6, day == 13) %>% 
  group_by(hour) %>% 
  summarize(
    precip = max(precip, na.rm = TRUE),
    wind_speed = max(wind_speed, na.rm = TRUE)
  )