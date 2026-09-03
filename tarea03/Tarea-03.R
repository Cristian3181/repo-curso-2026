#install.packages("palmerpenguins")
#install.packages("ggthemes")
#install.packages("tidyverse")

#Librerias para correr cada vez que abro R
library(ggthemes)
library(tidyverse)
library(palmerpenguins)
#mostrarme los datos de la libreria
penguins

#estructura, tipos de datos y un vistazo de estos
glimpse (penguins)

#creo un "lienzo" en blanco para mi gráfico
ggplot(data = penguins)

#Establecer los ejes y graficar los datos
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
)+
 geom_point()+  #el "+" sirve para procesar las lineas juntas
  #ahora con lineas de tendencia por especie 
  ggplot(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
  ) +
  geom_point() +
  geom_smooth(method = "lm")
  
#Linea de tendencia general
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species)) +
  geom_smooth(method = "lm")
#cambiamos los puntos de colores que van por especies a colores y formas
#produce una mayor distinción y prevención a errores interpretativos 
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm")

#agregado de titulos y subtitulos. Nombres de los ejes con espacios
#paleta de colores distinguibles para personas con daltonismo
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()

#Exercises
#1_How many rows are in penguins? How many columns?
dim(penguins)   #O glimpse, pero nos muestra más datos (species, islands)
#filas: 344, columnas: 8

#2_What does the bill_depth_mm variable in the penguins data frame describe?
#Read the help for ?penguins to find out.
?penguins  #a number denoting bill depth (millimeters)
#profundidad del pico medida en sentido vertical 
#(desde la parte superior del culmen hasta la base de la mandíbula inferior).

#3_Make a scatterplot of (...) variables.
ggplot(data = penguins, mapping = aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point()
#relación por especie
ggplot (
  data = penguins, 
  mapping = aes(x = bill_length_mm, y = bill_depth_mm, color = species)
) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
#Si vemos especie por especie, las 3 presentan una relacion positiva
#a mayor longitud, mayor profundidad de su pico. Si vemos la relación de las 
#tres especies juntas, los puntos generan una pendiente negativa, a mayor long. 
#menor es la profundidad si lo vemos sin distinción por especie. 

#4_What happens if you make a scatterplot of species vs. bill_depth_mm? What might be a better choice of geom?
ggplot(data = penguins, mapping = aes(x = species, y = bill_depth_mm)) +
 geom_point()  #no muestra la verdadera densidad y distribucion de los datos por especie
#box plot
ggplot(data = penguins, mapping = aes(x = species, y = bill_depth_mm)) +
  geom_boxplot()   #resumen estadistico: mediana, cuartiles y valores atipicos (outliers)
#grafico de densidad (2da alternativa)
ggplot(penguins, aes(x = body_mass_g, color = species)) +
  geom_density(linewidth = 0.75) 
#5
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point()  #geom_point exige posiciones espaciales (x,y)
#6
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(na.rm = TRUE) #Elimina los valores faltantes (NA) de forma silenciosa antes de dibujar los puntos
#7
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(na.rm = TRUE) +
  labs(caption = "Data come from the palmerpenguins package.") #Se añade la fuente al grafico
  
#8
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = bill_depth_mm)) +
  geom_smooth()
#bill_depth_mm a color y debe mapearse a nivel local dentro de geom_point()

#9
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)
) +
  geom_point() +
  geom_smooth(se = FALSE) #como color = island, cada isla tendrá un color
# Como el color depende de la isla, geom_smooth() separa los datos en 3 grupos independientes. 
#tres lineas de tendencia, una por isla

#10
# Gráf 1
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point() +
  geom_smooth()

# Gráf 2
ggplot() +
  geom_point(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_smooth(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g))

#son identicos
#En el gráfico 1, las capas geom_point() y geom_smooth() heredan automaticamente el "data" y el "mapping
#definidos en la llamada inicial a ggplot(). En el 2do, ambas capas reciben explicitamente exactamente los mismos
#datos y asignaciones de ejes

