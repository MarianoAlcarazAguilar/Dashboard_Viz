# Ahora vamos a hacer un mapa
# Inputs: Rango de años a considerar

# Filtramos el dataframe buscando solo los años que se hayan indicado
# Sumamos los puntos por equipo
# Coloreamos un punto del tamaño de los puntos ganadados y del color del equipo correspondiente


setwd("/Users/mariano/Documents/itam/visualizacion/Dashboard_viz")

library(leaflet)
library(dplyr)
library(sf)


# Lectura de los datos necesarios
circuits <- read.csv('./datos/circuits.csv')
historico <- read.csv('./datos/points_per_year.csv')
colores_equipo <- read.csv('datos/colores_equipos.csv')


# Definimos función para recibir años y regresar el dataframe para la gráfica
encontrar_mejor_equipo <- function(anio_inicio, anio_fin, n_top) {
  
  # Filtramos los datos para el rango de años especificado
  datos_filtrados <- filter(historico, year >= anio_inicio & year <= anio_fin)
  
  # Agrupamos los datos por circuitId y team
  # Calculamos suma total de points
  datos_agrupados <- datos_filtrados %>%
    group_by(circuitId, team) %>%
    summarise(total_points = sum(points))
  
  # Seleccionamos solo los renglones correspondientes con n_top
  datos_top <- datos_agrupados %>%
    group_by(circuitId) %>%
    top_n(n_top, total_points)
  
  # Juntamos los datos con los de circuitos para poderlos graficar
  resultado <- datos_top %>%
    merge(circuits, by = "circuitId") %>%
    merge(colores_equipo, by = "team")
  
  resultado$size_aux <- log(resultado$total_points) * 1.7
  
  return(resultado)
}

# Aquí es donde llamamos a la función filtrando por los años especificados
datos_filtrados <- encontrar_mejor_equipo(2000, 2020, 1)
# View(datos_filtrados)

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    data = datos_filtrados,
    lng = datos_filtrados$lng,
    lat = datos_filtrados$lat,
    radius = datos_filtrados$size_aux,
    color = datos_filtrados$color,
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = paste0(
      "<b>Circuito</b>: ", datos_filtrados$name, "<br>",
      "<b>Equipo</b>: ", datos_filtrados$team, "<br>",
      "<b>Puntos</b>: ", datos_filtrados$total_points
    )
  )
