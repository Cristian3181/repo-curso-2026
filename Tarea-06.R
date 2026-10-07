

# 1. CARGA DE LIBRERÍAS
library(edgar)       # Conexión directa a los servidores de la SEC
library(tidyverse)   # Manipulación de tablas y gráficos (dplyr, purrr, ggplot2)
library(tidytext)    # Minería de texto y tokenización

# 2. IDENTIFICACIÓN ANTE LA SEC (Obligatorio para evitar Error HTTP 403)
mi_contacto <- "Cristian Contrera 94CO47436032@campus.economicas.uba.ar"
options(HTTPUserAgent = mi_contacto)

# 3. Busqueda
cik_nvidia    <- "1045810"
cik_microsoft <- "789019"
anio_analisis <- 2024

terminos_capex <- c(
  "artificial intelligence", "data center", "graphics processing unit", 
  "commitments", "cloud computing", "uncertainty"
)

# 4. DESCARGA DE LOS REPORTES 10-K (Se guardan en la carpeta "edgar_Filings")
reporte_msft <- searchFilings(
  cik.no      = cik_microsoft, 
  form.type   = "10-K", 
  filing.year = anio_analisis, 
  word.list   = terminos_capex, 
  useragent   = mi_contacto
)

reporte_nvda <- searchFilings(
  cik.no      = cik_nvidia, 
  form.type   = "10-K", 
  filing.year = anio_analisis, 
  word.list   = terminos_capex, 
  useragent   = mi_contacto
)

# 5. Lectura en la memoria
archivos_10k <- tibble(
  empresa = c("Microsoft", "NVIDIA"),
  archivo = c(
    list.files("edgar_Filings", pattern = "789019.*\\.txt", full.names = TRUE,
               recursive = TRUE)[1],
    list.files("edgar_Filings", pattern = "1045810.*\\.txt", full.names = TRUE,
               recursive = TRUE)[1]
  )
) |> 
  filter(!is.na(archivo)) |>  
  mutate(texto = map_chr(archivo, read_file))

# 6. Limpieza y tokenizacion (Destrucción de código HTML/CSS/XBRL)
tokens_empresas <- archivos_10k |>
  mutate(
    texto = str_replace_all(texto, "(?is)<style.*?</style>", " "),
    texto = str_replace_all(texto, "<[^>]+>", " "),
    texto = str_replace_all(texto, "&[^;]+;", " ")
  ) |>
  unnest_tokens(word, texto) |>  
  #transforma el texto libre al formato Tidy (1 token = 1 fila)
  filter(str_detect(word, "^[a-z]{4,}$")) |>
  # <-- asegura que solo conservamos palabras del
  # alfabeto de 4 o más letras, eliminando códigos hexadecimales y ruido contable.
  anti_join(stop_words, by = "word") |>
  filter(!word %in% c("company", "financial", "services", "products", 
                      "including", "million", "billion", "msft", "nvda",
                      "december", "january", "february", "march", "april", 
                      "may", "june", "july", "august", "september", "october", 
                      "november", "fiscal", "year", "quarter", "months",
                      "ended", "total", "assets", "liabilities", "expense",
                      "sans", "nvidia", "hhhh", "parenttag", "contents",
                      "consolidatedstatementsofcashflows", "consolidatedbalancesheets",
                      "concentration", "prepaid", "narrative", "debtsecuritiesmember",
                      "microsoft"))   

# 7. TF-IDF
tfidf_empresas <- tokens_empresas |>
  # bind_tf_idf() calcula la frecuencia del término ponderada por la frecuencia 
  # inversa del documento. Penaliza matemáticamente las palabras que ambas 
  # empresas comparten y resalta el vocabulario idiosincrásico de cada negocio.
  count(empresa, word, sort = TRUE) |>
  bind_tf_idf(term = word, document = empresa, n = n) |>
  group_by(empresa) |>
  slice_max(tf_idf, n = 10, with_ties = FALSE) |>
  ungroup()

# Gráfico 1: TF-IDF
grafico_tfidf <- ggplot(tfidf_empresas, aes(x = tf_idf, y = reorder_within(word, tf_idf, empresa), fill = empresa)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~empresa, scales = "free_y") +
  scale_y_reordered() +
  labs(title = "Palabras Clave Idiosincrásicas (TF-IDF)", x = "Relevancia (tf-idf)", y = NULL) +
  theme_minimal()
print(grafico_tfidf)

# 8. Diccionario financiero, carga manual (Evita bloqueos de red en RStudio)
lexico_seguro <- bind_rows(
  tibble(word = c("risk", "uncertainty", "fluctuate", "contingency", "doubt", "volatility", "unpredictable", "depend"), sentiment = "uncertainty"),
  tibble(word = c("debt", "covenants", "commitments", "obligations", "repay", "restrict", "required", "pledged"), sentiment = "constraining"),
  tibble(word = c("litigation", "legal", "lawsuit", "plaintiff", "damages", "proceedings", "infringement", "court"), sentiment = "litigious"),
  tibble(word = c("loss", "decline", "adverse", "fail", "impairment", "penalty", "against", "damage"), sentiment = "negative")
)

# 9. Auditoria de riesgo
riesgo_burbuja <- tokens_empresas |>
  inner_join(lexico_seguro, by = "word") |>
  # inner_join() actúa como un filtro semántico: cruza nuestros tokens limpios 
  # con el léxico financiero y solo conserva las palabras que coinciden.
  count(empresa, sentiment, sort = TRUE) |>
  group_by(empresa) |>
  mutate(proporcion_riesgo = (n / sum(n)) * 100) |>
  # para evitar el sesgo si el reporte de una empresa es más largo que el de la otra
  ungroup()

# Gráfico 2: Perfil de Riesgo
grafico_riesgo <- ggplot(riesgo_burbuja, aes(x = sentiment, y = proporcion_riesgo, fill = empresa)) +
  geom_col(position = "dodge") +
  labs(title = "Auditoría de Riesgo Sistémico (Léxico Financiero Directo)",
       x = "Dimensión de Riesgo", y = "% sobre total de palabras analizadas") +
  theme_minimal()
print(grafico_riesgo)

# Quién asume la mayor carga de obligaciones financieras en el boom de la IA?
  # Microsoft lidera la categoria de riesgo "constraining" (limitacion de la empresa
  # economica: falta de liquidez o barreras en el mercado crediticio) con más del
  # 50% de sus palabras en esta categoria
  # Refleja enormes compromisos contractuales y deuda (commitments y obligations) 
  # para financiar el capital (CapEx) necesario. Con esto comunica de manera clara al
  # mercado que su gigantesca estrategia de infraestructura requiere comprometer flujos
  # de caja futuros, 

# Quién enfrenta mayor volatilidad y riesgo regulatorio (Incertidumbre y Litigios)?
  # Nvidia presenta casi el doble de palabras en la categorai "uncertainty" que microsoft
  # y en "litigious" tiene entre 3 a 4 puntos porcentuales más Esto indica que, al ser
  # el proveedor monopólico de chips, NVIDIA advierte a sus inversores sobre la alta 
  # volatilidad de la demanda (depende de que pocos clientes sigan comprando) y el 
  # creciente riesgo de escrutinio regulatorio o juicios por patentes
  # (litigation, infringement).