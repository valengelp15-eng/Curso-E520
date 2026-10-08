install.packages("tidytext")
install.packages(c("tidytext", "topicmodels", "igraph", "ggraph", "textdata"))
library(tidytext)
library(topicmodels)
library(igraph)
library(ggraph)
library(textdata)
df_mckinsey <- read_csv("DATA-T9-mckinsey-mind-the-gap-articles-20251020.csv")

colnames(df_mckinsey)
# 1. Cargamos el diccionario de palabras vacías (stop_words)
data("stop_words")

# 2. Tokenizar (separar en palabras) usando la columna 'article_text'
tokens_mckinsey <- df_mckinsey %>%
  unnest_tokens(word, article_text)

# 3. Quitar las stop words y los números sueltos para limpiar el corpus
tokens_limpios <- tokens_mckinsey %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word, "^[0-9]+$"))

# 4. Contar las palabras más frecuentes
frecuencias <- tokens_limpios %>%
  count(word, sort = TRUE)

# 5. Ver las 15 palabras más usadas
head(frecuencias, 15)

# --- 1. ¿De qué se trata este corpus y cuáles son los aspectos destacados? ---
# El corpus trata principalmente sobre la Generación Z ("gen", "zers") y su 
# relación con el mundo laboral y corporativo ("job", "employees", "people"). 
# Destacan aspectos vinculados a la tecnología ("ai"), la salud ("health") 
# y los consumidores. Además, tiene un tono institucional evidenciado por 
# la fuerte presencia de palabras como "mckinsey", "global", "partner" y "percent".

# --- 2. Análisis de Sentimiento ---

# Sentimiento Binario (Bing)
sentimiento_bing <- tokens_limpios %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment, sort = TRUE)

print(sentimiento_bing)

# Sentimiento con Graduaciones (AFINN)
sentimiento_afinn <- tokens_limpios %>%
  inner_join(get_sentiments("afinn")) %>%
  summarise(puntaje_total = sum(value),
            puntaje_promedio = mean(value))

print(sentimiento_afinn)

# --- 2. Análisis de Sentimiento ---
# Utilizando el diccionario binario (Bing), el sentimiento general del corpus 
# es mayoritariamente positivo, con 2418 palabras positivas frente a 1515 negativas.
# Utilizando el diccionario con graduaciones (AFINN), el resultado también es 
# positivo, con un puntaje total de 2320 y un promedio de 0.662 por palabra, 
# lo que confirma que el corpus tiene un tono general optimista o constructivo.

# --- 3. Topic Modelling (k=10 y k=15) ---

# 1. Contamos las palabras por cada documento (usamos el 'title' como identificador)
palabras_por_doc <- df_mckinsey %>%
  unnest_tokens(word, article_text) %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word, "^[0-9]+$")) %>%
  count(title, word)

# 2. Convertimos esto al formato de Matriz Documento-Término (DTM) que requiere el modelo
dtm_mckinsey <- palabras_por_doc %>%
  cast_dtm(title, word, n)

# 3. Entrenamos el modelo para k = 10 tópicos
modelo_lda_10 <- LDA(dtm_mckinsey, k = 10, control = list(seed = 1234))

# 4. Extraemos las 5 palabras más representativas de cada uno de los 10 tópicos
topicos_10 <- tidy(modelo_lda_10, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Imprimimos el resultado para ver los temas
print(topicos_10)
library(topicmodels)

# Extraemos las palabras clave del modelo que acabas de entrenar
topicos_10 <- tidy(modelo_lda_10, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Mostramos el resultado
print(topicos_10, n = 50)
install.packages("reshape2")
topicos_10 <- tidy(modelo_lda_10, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>%
  ungroup() %>%
  arrange(topic, -beta)

print(topicos_10, n = 50)
# --- Topic Modelling para k = 15 tópicos ---

# 1. Entrenamos el modelo indicando k = 15
modelo_lda_15 <- LDA(dtm_mckinsey, k = 15, control = list(seed = 1234))

# 2. Extraemos las 5 palabras más representativas de cada tópico
topicos_15 <- tidy(modelo_lda_15, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>%
  ungroup() %>%
  arrange(topic, -beta)

# 3. Imprimimos el resultado (n = 75 para ver las 5 palabras de los 15 temas)
print(topicos_15, n = 75)

# --- 3. Conclusión del Topic Modelling (k=10 y k=15) ---
# En ambos modelos, las palabras "gen" y "zers" dominan transversalmente 
# casi todos los grupos, ya que el corpus entero trata sobre la Generación Z.
# 
# - En el modelo de k=10: Surgen subtemas generales muy claros como deportes (tópico 4), 
#   empleo femenino y ambiente laboral (tópico 5: "women", "employees", "job"), 
#   salud y alimentación (tópico 6: "health", "food"), inteligencia artificial 
#   (tópico 7: "ai") y tendencias de consumo/moda (tópico 10: "fashion", "consumers").
# 
# - En el modelo de k=15: Al forzar más divisiones, los temas se vuelven más granulares 
#   y específicos. Se logran aislar tópicos muy puntuales como: salud mental 
#   (tópico 14: "mental", "health"), viajes y tiempo libre (tópico 13: "travel", "time"), 
#   educación (tópico 12: "school") y el cruce entre inteligencia artificial 
#   y desarrollo de habilidades (tópicos 7 y 11: "ai", "skills").

# --- 4. AVANZADO: Análisis de redes de bigrams ---

# 1. Separamos el texto en bigramas (pares de 2 palabras)
bigramas <- df_mckinsey %>%
  unnest_tokens(bigram, article_text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# ---------------------------------------------------------
# A. Red de bigramas más frecuentes
# ---------------------------------------------------------
bigramas_frecuentes <- bigramas %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!str_detect(word1, "^[0-9]+$")) %>%
  filter(!str_detect(word2, "^[0-9]+$")) %>%
  drop_na() %>%
  count(word1, word2, sort = TRUE)

set.seed(2026)
bigramas_frecuentes %>%
  slice_max(n, n = 30) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, edge_colour = "darkred") +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void() +
  labs(title = "Red de Bigramas Más Frecuentes")

# ---------------------------------------------------------
# B. Red de bigramas con negaciones ("NOT word", "NO word")
# ---------------------------------------------------------
bigramas_negacion <- bigramas %>%
  filter(word1 %in% c("not", "no", "never", "without")) %>%
  filter(!word2 %in% stop_words$word) %>%
  drop_na() %>%
  count(word1, word2, sort = TRUE)

set.seed(2026)
bigramas_negacion %>%
  filter(n > 2) %>% # Filtramos para que el gráfico no quede ilegible
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, edge_colour = "steelblue") +
  geom_node_point(color = "lightcoral", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void() +
  labs(title = "Red de Bigramas con Negaciones")
library(igraph)
library(ggraph)
# ---------------------------------------------------------
# B. Red de bigramas con negaciones (MEJORADA)
# ---------------------------------------------------------
set.seed(2026)
bigramas_negacion %>%
  filter(n > 2) %>% 
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  # Hacemos las líneas un poco más gruesas según la frecuencia
  geom_edge_link(aes(edge_width = n), alpha = 0.6, edge_colour = "gray50", show.legend = FALSE) +
  # Agrandamos los nodos para que el texto entre mejor
  geom_node_point(color = "lightcoral", size = 12) +
  # Centramos el texto DENTRO del nodo y le damos un color oscuro para que resalte
  geom_node_text(aes(label = name), color = "black", fontface = "bold", size = 4) +
  theme_void() +
  labs(title = "Red de Bigramas con Negaciones")

# --- 4. Conclusión de Redes de Bigramas ---
# En la red de bigramas más frecuentes destacan fuertemente las combinaciones 
# "gen zers", "mental health" y "social media", reforzando el perfil del corpus. 
# 
# En cuanto al análisis de negaciones, se identificaron construcciones como 
# "no matter", "no surprise" y "not necessarily". Estas estructuras indican 
# que los artículos de McKinsey probablemente están desmitificando creencias 
# previas sobre esta generación (ej. "not necessarily") o enfatizando 
# situaciones ineludibles en el ámbito laboral ("no matter").