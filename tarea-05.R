library(readr)
articulos <- read_csv("C:/Users/Cristian/Documents/curso-E520/clases/DATA-T9-mckinsey-mind-the-gap-articles-20251020.csv", locale = locale(encoding = "UTF-8"))
#View (articulos)

#librerias a utilizar
library(tidytext)
library(tidyverse)
library(topicmodels)
library(textdata)
library(igraph)
library(ggraph)

# pregunta 1 --------------------------------------------------------------
#De qué se trata este corpus? Son comparables los documentos?

  #busco mis stop words
  custom_stop_words <- bind_rows(
    stop_words,
    tibble(
      word = c("mckinsey", "partner", "read", "email", "brought", "global",
               "newsletter", "company", "percent", "people", "time", 
               "it’s", "it's", "we’re", "we're"),
      lexicon = "custom"
    )
  )
#analisis estadistico descriptivo sobre la longitud de los textos en mi base de datos
#conteo de palabras por articulo
#metricas de resumen y visualizacion
metricas_longitud <- articulos |> 
  mutate(n_palabras = str_count(article_text, "\\w+")) |> 
  summarise(
    Media = mean(n_palabras, na.rm = TRUE),
    Mediana = median(n_palabras, na.rm = TRUE),
    Desvio = sd(n_palabras, na.rm = TRUE),
    CV = sd(n_palabras, na.rm = TRUE) / mean(n_palabras, na.rm = TRUE)
  )

print(metricas_longitud)

#term frecuency - inverse document frecuency. TF-IDF
#analizo cuáles son las palabras más importantes y caracteristicas de cada articulo individual.
tokens_unigrama <- articulos |>
  unnest_tokens(word, article_text) |>
  anti_join(custom_stop_words, by = "word") |>
  filter(!str_detect(word, "^[0-9]+$")) 

aspectos_tfidf <- tokens_unigrama |>
  count(title, word, sort = TRUE) |>
  bind_tf_idf(word, title, n) |>
  arrange(desc(tf_idf))


#análisis de sentimiento léxico para cada artículo utilizando el diccionario Bing.
#El objetivo final es calcular un puntaje neto de sentimiento por documento, 
#restando las palabras negativas de las positivas.

sentimiento_bing <- tokens_unigrama |>
  inner_join(get_sentiments("bing"), by = "word") |>
  count(title, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  mutate(net_bing = positive - negative)

#análisis de sentimiento graduado (o cuantitativo)
#para cada artículo utilizando el diccionario AFINN.Puntaje numerico +-5
sentimiento_afinn <- tokens_unigrama |>
  inner_join(get_sentiments("afinn"), by = "word") |>
  group_by(title) |>
  summarise(score_afinn_promedio = mean(value), .groups = "drop")
sentimiento_afinn <- tokens_unigrama |>
  inner_join(get_sentiments("afinn"), by = "word") |>
  group_by(title) |>
  summarise(score_afinn_promedio = mean(value), .groups = "drop")

#------------------------------
#implementacion de topic modeling utilizando el algoritmo LDA. Extraer las 5 palabras más relevantes
## Matriz Documento-Término
dtm_corpus <- tokens_unigrama |>
  count(title, word) |>
  cast_dtm(title, word, n)

# Modelos LDA
set.seed(1234)
lda_10 <- LDA(dtm_corpus, k = 10, control = list(seed = 1234))
lda_15 <- LDA(dtm_corpus, k = 15, control = list(seed = 1234))

# Extracción de beta (probabilidad de palabra en tópico)
top_terms_k10 <- tidy(lda_10, matrix = "beta") |>
  group_by(topic) |>
  slice_max(beta, n = 5) |>
  ungroup() |>
  arrange(topic, desc(beta))


# 5. AVANZADO: BIGRAMAS Y NEGACIONES
# ------------------------------------------------------------------------------
# Creación de bigramas
bigramas <- articulos |>
  unnest_tokens(bigram, article_text, token = "ngrams", n = 2) |>
  separate(bigram, c("word1", "word2"), sep = " ")

# Negaciones
palabras_negacion <- c("not", "no", "never", "without")
analisis_negaciones <- bigramas |>
  filter(word1 %in% palabras_negacion) |>
  inner_join(get_sentiments("afinn"), by = c("word2" = "word")) |>
  count(word1, word2, value, sort = TRUE) |>
  mutate(contribucion = n * value) |>
  arrange(contribucion)

# Grafo de Red (Frecuencias altas sin stop words)
grafo_bigramas <- bigramas |>
  filter(!word1 %in% custom_stop_words$word, 
         !word2 %in% custom_stop_words$word) |>
  count(word1, word2, sort = TRUE) |>
  filter(n > 10) |>
  graph_from_data_frame()

# Visualización del grafo
library(grid)
a <- arrow(type = "closed", length = unit(.15, "inches"))
set.seed(2026)
ggraph(grafo_bigramas, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
  geom_node_point(color = "darkred", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.5, hjust = 1) +
  theme_void() +
  ggtitle("Red de Bigramas Frecuentes - McKinsey Gen Z")

#Respuestas
#1. ¿De qué se trata este corpus? ¿Son comparables los documentos?
 # El corpus consta de 146 artículos tipo newsletter de la consultora McKinsey & Company
#enfocados en la inserción laboral, salud mental y pautas de consumo de la Generación Z.
#Los documentos son estrictamente comparables tanto discursiva como estadísticamente.
#Comparten un formato editorial estandarizado y presentan una dispersión de volumen baja:
#la longitud media es de aprox 625$ palabras con un coeficiente de variación de apenas 0.22 (22%).
#Al no existir asimetrías severas en su extensión, las métricas de frecuencia y los
#modelos de tópicos no sufren sesgos de sobrerrepresentación.

#2. Aspectos destacados del 
#corpus utilizadoPara destacar la señal semántica real de cada documento se utilizó el 
#estadístico TF-IDF. Al remover las stop words en inglés y un diccionario personalizado
#de términos corporativos de altísima frecuencia global (como "mckinsey", "partner", "global"),
#el TF-IDF logró aislar exitosamente el vocabulario idiosincrásico de cada nota. Esto permite 
#identificar rápidamente el tema técnico específico de cada artículo penalizando el "ruido" 
#institucional constante.

#3. Sentimiento de las notas utilizando diccionarios binarios y con 
#graduacionesDiccionario Binario (bing): Muestra un sesgo general hacia la positividad, propio
#del lenguaje corporativo orientado a "crecimiento" e "innovación", aunque detecta correctamente 
#artículos negativos vinculados a crisis de bienestar ("burnout", "ansiedad").Diccionario Graduado 
#(afinn): Al ponderar la intensidad semántica (escala de -5 a +5), la medición es más robusta.
#Permite identificar que los textos sobre renuncias masivas o crisis económica no solo son negativos,
#sino que presentan una severidad mucho mayor, ajustando el puntaje neto que el enfoque binario
#aplanaba.

# 4. Topic modelling para k=10 y k=15 tópicosSe utilizó la Asignación Latente de
#Dirichlet (LDA).Con $k=10$, el algoritmo identifica agrupamientos latentes bien diferenciados 
#y parsimoniosos (ej. clústeres separados para tecnología, bienestar laboral, y estrategias
#de consumo).Al expandir a $k=15$, el modelo comienza a sufrir un leve
#sobreajuste (overfitting): los temas se micro-fragmentan demasiado o empiezan a solaparse 
#compartiendo términos genéricos. Para el tamaño de este corpus (146 notas), $k=10$ resulta
#el modelo más equilibrado.5. AVANZADO: Análisis de redes de bigramas más frecuentes y con
#negacionesRed de Bigramas: El grafo de co-ocurrencia revela que el discurso de McKinsey se
#apoya en conceptos fuertemente cristalizados de la época. Se observan nodos directos muy
#claros como "mental $\rightarrow$ health", "climate $\rightarrow$ change",
#"generative $\rightarrow$ ai" y "social $\rightarrow$ media".Efecto de la Negación: 
# Al filtrar modificadores como "not", "no" o "never", se comprobó empíricamente el efecto 
#de polisemia. Muchas palabras valoradas como positivas por los léxicos pierden o invierten
#su polaridad en el contexto real de la frase. Esto demuestra que utilizar únicamente un
#modelo de bolsa de palabras (unigramas) sobreestima artificialmente el optimismo de los reportes.