# ==============================================================================
# ECON 520 - Ciencia de Datos para Economía y Negocios
# Tarea 02 - R Base (Programación aplicada sobre base macroeconómica)
# Alumno: Cristian Contrera
# Base utilizada: 'longley' (Macroeconomía y Empleo, integrada en R Base)
# ==============================================================================

#Cargamos una base de datos que R ya contenga
data(longley)
datos_macro <- longley
#Cómo está constituida la base de datos y un resumen estadistico
str(datos_macro)
summary(datos_macro)

# 2. Types: Tipos de datos y operadores

# Extraemos valores reales del primer registro (fila 1, [1]) de la base 'longley'
#anio para no usar ñ y generar errores
anio_inicio <- as.integer(longley$Year[1])        # Convertimos el año a integer (1947L).Dato entero
desempleo_inicial <- longley$Unemployed[1]        # Extraemos desempleo (235.6 -> numeric)
nombre_variable <- "Unemployed"                   # Nombre de la columna -> character
es_serie_anual <- TRUE                            # Indicador --> logical

# Ejecutamos class() para que R nos confirme en la consola el tipo de cada variable
class(anio_inicio)        # Devuelve: "integer"
class(desempleo_inicial)  # Devuelve: "numeric"
class(nombre_variable)    # Devuelve: "character"
class(es_serie_anual)     # Devuelve: "logical"
#class(periodo_nombre)  no lo va a encontrar porque lo renombré   

# Manipulación de cadenas de texto
#paste() hace el trabajo de "armar" la frase.
etiqueta <- paste("Analisis Macro - Variable central:", nombre_variable)
print(etiqueta)

# Operadores de comparación y lógicos
# Identificamos registros con desempleo mayor a 300 y empleo total mayor a 65
filtro_crisis <- (datos_macro$Unemployed > 300) & (datos_macro$Employed > 65)
#Voy a usar ñ, aunque habria que evitarlo por si no tiene UTF-8
print(paste("Años bajo condicion critica:", sum(filtro_crisis))) 

#Al ejecutar sum(filtro_crisis), R no está sumando los niveles de desempleo,
#sino que está contando cuántos TRUE hay en total."años (...) critica: 5"


# ------------------------------------------------------------------------------
# 3. Data Structures: Vectores, Factores, Matrices y Listas
# ------------------------------------------------------------------------------
# Vector numérico y medidas estadísticas
desempleo_vec <- datos_macro$Unemployed  #estoy creando una variable y guardando datos dentro de ella
media_desempleo <- mean(desempleo_vec)   #Si solo pongo lo 2do, lo calcula y lo olvida. 
                                         #Creando una variable lo calcula y lo guarda allí

# Factor ordinal (jerarquia) creado a partir del PNB (GNP)
nivel_pnb <- ifelse(datos_macro$GNP > mean(datos_macro$GNP), "Alto", "Bajo")
#si el PNB es mayor a la media, pone "Alto", sino bajo
pnb_factor <- factor(nivel_pnb, levels = c("Bajo", "Alto"), ordered = TRUE)
#indico con "levels = c" cuales son los escalones/orden
levels(pnb_factor)

# Matriz 2x2 con métricas de resumen (media, desvio estandar)
matriz_resumen <- matrix(
  c(mean(datos_macro$GNP), sd(datos_macro$GNP),
    mean(datos_macro$Unemployed), sd(datos_macro$Unemployed)),
  nrow = 2,
  byrow = TRUE,   
  dimnames = list(c("GNP", "Unemployed"), c("Media", "Desvio"))
)  #nombre de las filas y columnas
print(matriz_resumen)


# 4. Conditionals & Flow Control: If/Else, Bucles For y While
# ------------------------------------------------------------------------------
# If...Else sobre el último año registrado
ultimo_desempleo <- tail(datos_macro$Unemployed, 1)

if (ultimo_desempleo > 400) {
  alerta <- "Alerta: Desempleo elevado"
} else {
  alerta <- "Nivel de desempleo controlado"
}         #toma decisión basado en un umbral critico (>400)
print(alerta)

# Bucle For: cálculo de variación interanual porcentual del GNP
var_pnb <- numeric(nrow(datos_macro))

for (i in 2:nrow(datos_macro)) {
  var_pnb[i] <- ((datos_macro$GNP[i] - datos_macro$GNP[i - 1]) / datos_macro$GNP[i - 1]) * 100
}
datos_macro$Crecimiento_GNP <- var_pnb

# Bucle While con break: Búsqueda del primer registro donde Employed >= 65
idx <- 1
while (idx <= nrow(datos_macro)) {
  if (datos_macro$Employed[idx] >= 65) {
    print(paste("Umbral de 65 alcanzado en el registro:", rownames(datos_macro)[idx]))
    break
  }
  idx <- idx + 1
}

# Lista heterogénea final (agrupa distintos tipos de objetos)
resumen_final <- list(
  datos = datos_macro,
  matriz = matriz_resumen,
  factor_pnb = pnb_factor
) 
