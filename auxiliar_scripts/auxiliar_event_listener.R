library(shiny)
library(leaflet)
# Interfaz de usuario
# El primer argumento de column es el ancho de la columna
# Despliega mapa y gráfica
ui <- fluidPage(
  br(),
  column(8,leafletOutput("miMapa", height="600px")),
  column(4,plotOutput("grafica", height="300px")),
  br()
)
server <- function(input, output) {
  # Dataframe con dos lugares
  data=data.frame(x=c(-99.2, -100), y=c(19.34,25), id=c("Lugar1", "Lugar2"))
  # Va a almacenar la posición del marcador donde se hizo clic
  data_of_click <- reactiveValues(clickedMarker=NULL)
  # Leaflet con dos marcadores, posicionado en México
  output$miMapa <- renderLeaflet({
    leaflet() %>%
      setView(lng=-99.2 , lat =19.34, zoom=4) %>%
      addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(data=data, ~x , ~y, layerId=~id, popup=~id, radius=8 ,
                       color="black", fillColor="red", stroke = TRUE, fillOpacity = 0.8)
  })
  # Almacena el clic
  # Se trata de variables internas
  observeEvent(input$miMapa_marker_click,{
    data_of_click$clickedMarker <- input$miMapa_marker_click
    
  })
  # Haz un scatterplot o un barplot dependiendo del punto seleccionado
  output$grafica=renderPlot({
    my_place=data_of_click$clickedMarker$id
    if(is.null(my_place)){my_place="Lugar1"}
    if(my_place=="Lugar1"){
      plot(rnorm(1000), col=rgb(0.9,0.4,0.1,0.3), cex=3, pch=20)
    }else{
      barplot(rnorm(10), col=rgb(0.1,0.4,0.9,0.3))
    }
  })
}
shinyApp(ui = ui, server = server)
