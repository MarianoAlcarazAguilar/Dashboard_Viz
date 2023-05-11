setwd("/Users/mariano/Documents/itam/visualizacion/proytecto/scripts")
library(leaflet)
library(ggplot2)
library(shiny)
library(dplyr)
library(DT)

circuits <- read.csv("./circuits.csv")
circuits_count <- as.data.frame(table(circuits$country))
circuits_count <- circuits_count[order(circuits_count$Freq, decreasing = TRUE), ]

# Definir la UI
ui <- navbarPage(
  "Mi aplicación",
  # Primera página
  tabPanel("Mundo",
           plotOutput("graf1"),
           leafletOutput("mapa")
  ),
  # Segunda página
  tabPanel("Tabla",
           DTOutput("tabla")
  )
)


# Definir el servidor
server <- function(input, output) {
  output$graf1 <- renderPlot({
    ggplot(circuits_count, aes(x = reorder(Var1, Freq), y = Freq)) +
      geom_bar(stat = "identity", fill = "#3671C6") + 
      coord_flip() + 
      scale_y_continuous(limits = c(0,12),n.breaks = 12) +
      xlab("País") +
      ylab("Número de circuitos") +
      ggtitle("Número de circuitos por país")+
      theme_bw()
  })
  
  output$mapa <- renderLeaflet({
    leaflet(circuits) %>%
      addTiles() %>%
      addMarkers(
        data=circuits,
        popup = paste0(
          "<strong>Nombre: </strong>", circuits$name, "<br>",
          "<strong>Ubicación: </strong>", circuits$location, "<br>"
        ),
        clusterOptions = markerClusterOptions()
      )
  })
  
  # Segunda página
  output$tabla <- renderDT({
    # Mostrar la tabla de mtcars con opciones de búsqueda, paginación y ordenamiento
    circuits_count
  })
}

# Crear la aplicación
shinyApp(ui, server)
