library(nycflights13)
library(tidyverse)
#view(flights) #ventana de todos los datos, exploro filas y col
glimpse(flights)
# print(flights, width = Inf)

# pruebo filter
flights |> 
  filter(dep_delay < 5, dep_delay > 0) |> 
  filter(month == 2, day == 1) 

flights |>    
  filter(month %in% c(3, 4, 5), day %in% c(2, 3, 4)) # %in% para no escribir tanto o (|)

# demoras promedio a Houston por dia
flights |> 
  filter(dest == "IAH") |>  #IAH aeropuerto
  group_by(year, month, day) |> 
  summarise(
    arr_delay = mean(arr_delay, na.rm = TRUE),
    dep_delay = mean(dep_delay, na.rm = TRUE),
    .groups = "drop"
  )

# vuelos unicos por origen y destino
flights |> 
  distinct(origin, dest, .keep_all = TRUE) |> 
  arrange(year, month, day, dep_time, dep_delay)

# rutas más transitadas
flights |> 
  count(origin, dest, sort = TRUE)

# pruebo crear variables de costos modelados, creando columnas
flights |> 
  mutate(
    tardanza_total = dep_delay + arr_delay,
    precio_pasaje = 150 + distance * 0.15,
    precio_milla = precio_pasaje / distance,
    costo_combustible = distance * 19 * 1.13, #Total gastado por distancia. 19L por milla (estimativo)
    .before = 1
  )

# selecciono columnas
flights |> 
  select(year:sched_dep_time) #intervalo cerrado

# solo las que tienen texto, analizamos si hay errores escritos mirando esas columnas
flights |> 
  select(where(is.character))

# renombro carrier a Carrier sin perder las demas
flights |> 
  rename(Carrier = carrier)

# muevo las fechas despues de origin
flights |> 
  relocate(year:day,
           .after = origin)

library(tidyverse)
library(nycflights13)

# 19.2.4 Exercises (Keys)
# ====================================================

# 1_relacion entre weather y airports (Figura 19.1)
# airports tiene cada aeropuerto 1 sola vez (su faa es la primary key)
# weather tiene un monton de mediciones y usa "origin" para decir de que aeropuerto es
# en el diagrama la flecha iria de weather$origin a airports$faa (relacion de muchos a 1)

# 2_si weather tuviera datos de todo USA, que conexion sumamos?
# podriamos hacer un join con el destino de los vuelos (dest), no solo con el origen.
# nos serviria un monton para ver si un vuelo se demoro al llegar (arr_delay) 
# por culpa de una tormenta o nevada en la ciudad de destino.

# 3_el duplicado raro en weather
weather |>
  count(year, month, day, hour, origin) |>
  filter(n > 1)
# salta que hay un duplicado el 3 de noviembre a la 1 AM.
# que paso? el cambio de horario de invierno yankee. a las 2AM atrasan el reloj a la 1AM,
# asi que la hora 1 pasa dos veces la misma noche (una con horario de verano y otra normal)

# 4_dias especiales como navidad, ¿como armar la tabla?
# armariamos una tabla nueva (ej: feriados) con columnas: year, month, day, nombre_feriado
# la primary key seria la fecha en si (la combinacion year, month, day)
# se conecta a flights haciendo un left_join por year, month y day asi a cada vuelo le
# aparece al lado si cayo en un feriado o no.

# 5_Diagramas de Lahman package
# Batting y Salaries se conectan a People. People es la tabla madre (su playerID es la clave)
# Managers se conecta a People. AwardsManagers se conecta a Managers por ID y año.
# Batting, Pitching y Fielding: no dependen una de otra, son paralelas. un jugador batea,
# lanza y ataja en el mismo año. se unen por jugador, año (yearID) y equipo (teamID).


# =================================================
# 19.3.4 Exercises (Basic Joins)
# ===================================================

# 1 peores 48 hs de demoras y como "matchean" con el clima
peores_48 <- flights |>
  group_by(time_hour) |>
  summarise(demora = mean(dep_delay, na.rm = TRUE)) |>
  slice_max(demora, n = 48)

# si cruzo esto con weather (inner_join por time_hour) se ve un re patron:
# la visibilidad baja un monton, hay fuertes lluvias (precip) y rafagas de viento.
# casi siempre coinciden con tormentas de verano o nevadas de invierno.

# 2_vuelos a los 10 destinos mas populares
top_dest <- flights |> count(dest, sort = TRUE) |> head(10)
# uso semi_join, filtra la tabla grande usando las coincidencias de la chiquita
vuelos_top <- flights |> semi_join(top_dest, by = "dest")

# 3_¿todo vuelo tiene datos de clima en esa hora?
vuelos_sin_clima <- flights |> anti_join(weather, by = c("year", "month", "day", "hour", "origin"))
# no, hay vuelos que no cruzan. a veces los sensores del aeropuerto se caen, 
# o a la madrugada no reportan info.


# 4_aviones sin datos en planes (aviones huerfanos)
aviones_huerfanos <- flights |> anti_join(planes, by = "tailnum")
aviones_huerfanos |> count(carrier, sort = TRUE)
# al mirar esto salta a la vista: American Airlines (AA) y Envoy (MQ) son casi 
# el 90% de los faltantes. Pasa porque esas aerolineas no reportan los numeros 
# de cola estandar o usan una flota que no esta en el registro civil abierto.

# 5_¿cada avion vuela para una sola aerolinea?
flights |>
  filter(!is.na(tailnum)) |>
  distinct(tailnum, carrier) |>
  count(tailnum) |>
  filter(n > 1)
# hipotesis rechazada. si corres esto ves que hay 17 aviones que volaron para 
# mas de una aerolinea (suele pasar por prestamos de aviones o fusiones de empresas).

# 6_latitud y longitud en flights
# conviene mil veces renombrar ANTES del join. si no, R te pone lat.x o lat.y y 
# despues te haces un lio para saber cual era el origen y cual el destino.

# 7_mapa de demoras promedio por destino
demora_dest <- flights |>
  group_by(dest) |>
  summarise(promedio = mean(arr_delay, na.rm = TRUE))

# cruzo airports con las demoras para tener lat y lon, y lo grafico
airports |>
  semi_join(flights, by = c("faa" = "dest")) |>
  inner_join(demora_dest, by = c("faa" = "dest")) |>
  ggplot(aes(x = lon, y = lat, color = promedio, size = promedio)) +
  borders("state") +
  geom_point(alpha = 0.7) +
  coord_quickmap() +
  scale_color_viridis_c() # le da una paleta linda

# 8_¿que paso el 13 de junio de 2013?
# haces el mismo mapa de arriba pero filtrando flights (year == 2013, month == 6, day == 13)
# vas a ver puntos gigantes (demoras brutales) en todo el este de USA.
# al googlear salta la data: hubo un "derecho" (un complejo enorme de tormentas convectivas
# y tornados) que cruzo desde el centro a la costa este y obligo a cancelar todo.