#install.packages("igraphdata")
library(igraph)          # "calculadora de redes"
library(igraphdata)      
data(immuno)
g <- immuno

# Genera una radiografia estructural abreviada del grafo para entender su 
# tamaño y sus propiedades basicas sin saturar la vista con miles de conexiones
summary(g)
#IGRAPH 46d8c3f U--- 1316 6300 -- immunoglobuline network

# 1.2 ¿Qué atributos tienen los nodos y las aristas? Es dirigida o no dirigida? 
# Es ponderada o no ponderada?

# 1era posicion ("U"): undirected. Son conexiones bidireccionales (relacion mutua)
# 2da pos ("-"): los nodos no tienen nombres asignados internamente. Sino aparece "N" 
# 3era pos ("-"): las aristas no tienen un peso numerico asignado en weight. Sino "W"
# 3ta pos ("-"): No es una red bipartita
# 1316 es el numero total de nodos que componen la red
# 6300 es el numero total de aristas existentes entre esos nodos

#1.1cuantos nodos y aristas tiene?
vcount(g) #1316
ecount(g) #6300

# Atributos (ej. si los nodos tienen nombres asignados)
vertex_attr_names(g) # 0
edge_attr_names(g)   # 0
V(g)[1:10]$Name #Null  no hay ninguna "caja" de información de donde extraer datos.
V(g)[1:10]

# 1.3 ¿La red está conectada? Si no, ¿cuántos componentes tiene
# y qué tamaño tiene el más grande?
is_connected(g) #TRUE, está conectada
#la red es un grafo conexo perfecto (si se corta un enlace la comunicacion no se interrumpe)
#Tiene un unico componente (toda la red es una gran "isla") de 1316 nodos

#-------------------------------------------------------------
# COMO SE CONECTA?
grados <- degree(g)
mean(grados)   # Grado promedio (9.574468)
median(grados) # Mediana (10)
# 2.1
# Son muy similares. Al ser "mean" (9.57) casi igual a "median" (10), 
# la distribución es prácticamente simétrica (con una levísima inclinación a la izquierda).
# La mayoría de los nodos se concentran alrededor de esos valores centrales (9 y 10).
# Esto quiere decir que NO sigue una distribución "Power Law" (no hay cola larga). 
# Al contrario, apunta a una distribución homogénea (tipo Poisson), donde casi todos 
# los nodos tienen una cantidad de conexiones similar y no se ven indicios de tener 
# "hubs" o superconectores que alteren drásticamente el promedio.

hist(grados, breaks = "FD", col = "darkcyan", 
     main = "Distribución de Grados - Red Inmuno", 
     xlab = "Cantidad de Conexiones (Grados)", ylab = "Frecuencia")
#2.2
# No tiene cola larga, sino forma de Campana de la distr. Poisson. La mayoria tiene 
# entre 7 a 12 conexiones, con poca frecuencia en los extremos (>= 110)

# 2.3 Cuáles son los 5 hubs principales?
head(sort(grados, decreasing = TRUE), 5) #17 16 16 15 15
# En biología estructural, aunque esta red no tiene "hubs" gigantes (el máximo es 17), 
# estos nodos con mayor conectividad representan aminoácidos críticos para el plegamiento. 
# Actúan como puntos de anclaje estructural; si una mutación altera estos residuos clave, 
# el núcleo de la proteína puede desestabilizarse y perder su función.

#Tiene sentido que sean hubs?
# No en el sentido matematico. Un "hub" es un nodo con una cantidad descomunal de 
# conexiones en relación a la cantidad total de nodos. Lo cual, en nuestro caso con
# una distr. Poisson, no sería matemat. correcto hablar de hub. 
# En la realidad tridimensional de una proteína (como esta red de inmunoglobulina), 
# los nodos son aminoácidos y los enlaces son contactos físicos cercanos 
# (fuerzas de Van der Waals, puentes de hidrógeno, etc.).

# El tamaño limitado del aminoacido solo permite que su maximo de conexiones sea 
# de 15 a 18. Estos nodos máximos representan los residuos completamente enterrados 
# en el núcleo denso (core) de la proteína.

edge_density(g)
#2.3
# Resultado: ~0.00728 (0.73%). Es una red extremadamente esparsa. 
# En la naturaleza, mantener enlaces consume energía y, además, los aminoácidos 
# están limitados por el espacio físico tridimensional, optimizando la red 
# al conectar estrechamente solo con su entorno local inmediato.

#-----------------------------------------------------------------
# Coeficiente de clustering global (proporción de tríadas cerradas)
transitivity(g, type = "global")
#3.1
# Resultado: 0.4851166. A pesar de ser una red esparsa (0.73% de densidad), casi el 50% 
# de los vecinos de un nodo también están conectados entre sí. Esto demuestra una 
# altísima cohesión local; los aminoácidos forman empaquetamientos tridimensionales 
# muy compactos (módulos o vecindarios estructurados).
# Coeficiente de clustering global extremadamente alto
# Si tu red fuera puramente aleatoria, su clustering debería ser cercano al 0.007 
# (0.7%). Al darte 0.49 (49%), significa que el agrupamiento local de tu Red Inmuno 
# es unas 70 veces mayor de lo esperado por azar.

# Distancia promedio (Caminos cortos)
mean_distance(g)
#3.2
# La distancia promedio es de 16.01649. Es una distancia promedio alta para el tamaño
#de la red. 
# Esto nos indica que la proteína tiene una estructura física alargada o extendida, 
# donde la información o las fuerzas mecánicas tardan muchos "pasos" en viajar 
# desde un extremo del anticuerpo hasta el otro.



comunidades <- cluster_louvain(as.undirected(g))

length(comunidades) # Cantidad de comunidades encontradas
sizes(comunidades)  # Tamaño de cada una
#3.3
# Resultado: 13 comunidades en total. Lo más impresionante es la homogeneidad: 
# la gran mayoría (10 comunidades) tienen tamaños casi idénticos de ~100 a 120 nodos.
# Esto encaja perfecto con nuestra distribución de Poisson: no hay una mega-comunidad 
# que domine la red, sino bloques modulares muy equitativos.
# En el anticuerpo, estas 13 comunidades representan los "dominios estructurales" 
# o plegamientos locales compactos que cooperan de forma independiente.