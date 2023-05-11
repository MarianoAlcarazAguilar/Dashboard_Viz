# Ahora, dados dos años y un circuito id
# Se desea crear un barplot horizontal con los puntos por cada equipo conseguidos en ese rango
# de tiempo

library(plotly)
library(ggplot2)
library(sp)

id_circuit <- 32
anio_inicio <- 2010
anio_fin <- 2022

get_plotly_graph <- function(anio_inicio, anio_fin, circuit_id) {
  datos_filtrados <- filter(historico, year >= anio_inicio & year <= anio_fin & circuitId == circuit_id)
  circuit_name <- filter(circuits, circuitId == circuit_id)$name
  
  datos_graph <- datos_filtrados %>%
    group_by(team) %>%
    summarise(total_points = sum(points)) %>%
    merge(colores_equipo, by = "team") %>%
    arrange(desc(total_points))
  
  fig <- plot_ly(
    data = datos_graph,
    y = ~reorder(team, total_points),
    x = ~total_points,
    type = "bar",
    marker = list(color = ~color)
  ) %>%
    layout(
      xaxis = list(title = "Puntos Acumulados"),
      yaxis = list(title = "Equipo"),
      title = circuit_name
    )
  
  return(fig)
}

datos_filtrados <- filter(historico, year >= anio_inicio & year <= anio_fin & circuitId == id_circuit)
circuit_name <- filter(circuits, circuitId == id_circuit)$name

datos_graph <- datos_filtrados %>%
  group_by(team) %>%
  summarise(total_points = sum(points)) %>%
  merge(colores_equipo, by = "team") %>%
  arrange(desc(total_points))

g <- ggplot(datos_graph, aes(y = total_points, x = team, fill = color)) +
  geom_bar(stat = "identity") +
  coord_flip()
ggplotly(g)

? plot_ly
fig <- plot_ly(
  data = datos_graph,
  y = ~reorder(team, total_points),
  x = ~total_points,
  type = "bar",
  marker = list(color = ~color)
)

get_plotly_graph(2022, 2022, 32)


get_plotly_graph <- function(anio_inicio, anio_fin, circuit_id) {
  datos_filtrados <- filter(historico, year >= anio_inicio & year <= anio_fin & circuitId == circuit_id)
  circuit_name <- filter(circuits, circuitId == circuit_id)$name
  
  datos_graph <- datos_filtrados %>%
    group_by(team) %>%
    summarise(total_points = sum(points)) %>%
    merge(colores_equipo, by = "team") %>%
    arrange(desc(total_points))
  
  fig <- plot_ly(
    data = datos_graph,
    y = ~reorder(team, total_points),
    x = ~total_points,
    type = "bar",
    marker = list(color = ~color)
  ) %>%
    layout(
      xaxis = list(title = "Puntos Acumulados"),
      yaxis = list(title = "Equipo"),
      title = circuit_name
    )
  
  return(fig)
}
