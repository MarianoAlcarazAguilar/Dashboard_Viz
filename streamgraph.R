setwd("/Users/mariano/Documents/itam/visualizacion/Dashboard_viz")

library(ggplot2)
library(ggstream)


data <- read.csv("datos/data_for_streamgraph.csv")
datos_stack <- read.csv("datos/data_for_streamgraph.csv")
colores_equipo <- read.csv("./datos/colores_equipos.csv")

# Crear el streamgraph
s <- ggplot(datos_stack, aes(x = year, y = porcentaje, fill = team)) +
  geom_stream(alpha = 1, linewidth = 0, color = "black") +
  scale_fill_manual(values = c("Red Bull"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD",
                               "Alpine"="#2293D1","Haas"="#B6BABD","AlphaTauri"="#5E8FAA","McLaren"="#F58020","Alfa Romeo"="#C92D4B")) +
  labs(x = "", y = "Puntos", fill = "Equipo") +
  theme_minimal() +
  theme(legend.position = "top")
s

# Crear la gráfica de barras apiladas
g <- ggplot(data_graph, aes(x = year, y = porcentaje, fill = team)) +
  geom_bar(stat = "identity", width = 0.95) +
  scale_fill_manual(values = c("Red Bull"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD",
                               "Alpine"="#2293D1","Haas"="#B6BABD","AlphaTauri"="#5E8FAA","McLaren"="#F58020","Alfa Romeo"="#C92D4B")) +
  labs(x = "", y = "Valor", fill = "Grupo") +
  theme_minimal() +
  theme(legend.position = "top")


ggplotly(g)

get_stacked_plot <- function(anio_inicio, anio_fin) {
  data_graph <- filter(datos_stack, year >= anio_inicio & year <= anio_fin)
  
  g <- ggplot(data_graph, aes(x = year, y = porcentaje, fill = team)) +
    geom_bar(stat = "identity", width = 0.95) +
    scale_fill_manual(values = c("Red Bull"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD",
                                 "Alpine"="#2293D1","Haas"="#B6BABD","AlphaTauri"="#5E8FAA","McLaren"="#F58020","Alfa Romeo"="#C92D4B")) +
    labs(x = "", y = "Valor", fill = "Grupo") +
    theme_minimal() +
    theme(legend.position = "top")
  
  return(ggplotly(g))
  
}
get_stacked_plot(2010, 2015)


